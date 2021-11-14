module Spawning where

import World
import Control.Monad.State
import System.Random.Stateful
import Control.Lens

import Config
import Being
import EnemyAI

{- 
 - This module handles the spawning of new enemies
 - We use randomness for the location
 - Also there are some number to make the spawn location nice
 - Also here is determined when certain enemies start spawning
 -}
 

uniformFloat :: Float -> Float -> State StdGen Float
uniformFloat l h = state $ uniformR (l, h)

uniformBool :: State StdGen Bool
uniformBool = state uniform


-- rates as in T ~ Exp(1/t), t in spawnTicks
toRate :: Float -> Float 
toRate interval = perTick where
    perSecond = 1 / interval
    perTick = perSecond * spawnTick

baseSpawnRates :: SpawnData
baseSpawnRates = SpawnData 0 (toRate secondsPerAsteroid) (toRate secondsPerEnemy) (toRate secondsPerChaser)

spawnStep :: Float -> World -> World
spawnStep dt = execState $ do
    spawns . timeSinceLast += dt
    t <- use (spawns . timeSinceLast)
    (w, h) <- use frame

    let n = round (t / spawnTick)
    spawns . timeSinceLast -= fromIntegral n * spawnTick

    forM_ [1..n] $ \_ -> do -- lol
        aRate <- use (spawns . asteroidRate)
        spawnRoll w aRate Asteroid 
        cRate <- use (spawns . chaserRate)
        spawnRoll w aRate Chaser
        eRate <- use (spawns . enemyRate)
        spawnRoll w eRate (Enemy  aimAtAI floatAI)


spawnRoll :: Float -> Float -> Race -> StateT World Identity ()
spawnRoll w rate what = do
        ret <- zoom randomizer $ do
            roll <- uniformFloat 0.0 1.0

            if roll < rate then do
                x  <- uniformFloat (-w) w
                vx <- uniformFloat spawnVXMin spawnVXMax
                vy <- uniformFloat spawnVYMin spawnVYMax
                let y = 7 * fromIntegral windowHeight / 16
                spawnfromabove <- uniformBool
                if spawnfromabove then return $ Just (x, y, vx, vy)
                else return $ Just (x, -y, vx, -vy)
            else return Nothing

        case ret of
            Just (x, y, vx, vy) -> spawnBeing (makeBeing what (x, y) (vx, vy))
            Nothing -> return ()
