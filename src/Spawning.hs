module Spawning where

import World
import Control.Monad.State
import Control.Lens

import Util
import Being


spawnTick :: Float
spawnTick = 0.1 -- seconds

-- rates as in T ~ Exp(1/t), t in spawnTicks
toRate :: Float -> Float -- TODO I think this doesn't mean what I think it does
toRate interval = perTick where
    perSecond = 1 / interval
    perTick = perSecond * spawnTick


baseSpawnRates :: SpawnData
baseSpawnRates = SpawnData 0 (toRate 2.0) (toRate 2.0)


spawnStep :: Float -> World -> World
spawnStep dt = execState $ do
    spawns . timeSinceLast += dt
    t <- use (spawns . timeSinceLast)
    (w, h) <- use frame
    -- put enemy here too

    let n = round (t / spawnTick)
    spawns . timeSinceLast -= fromIntegral n * spawnTick

    forM_ [1..n] $ \_ -> do -- lol
        aRate <- use (spawns . asteroidRate)
        spawnRoll w aRate Asteroid 
        eRate <- use (spawns . enemyRate)
        spawnRoll w eRate (Enemy  aimAtAI floatAI)

spawnRoll :: Float -> Float -> Race -> StateT World Identity ()
spawnRoll w rate what = do
        ret <- zoom randomizer $ do
            roll <- uniformF 0.0 1.0

            if roll < rate then do
                x <- uniformF (-w) w
                vx <- uniformF (-20) 20
                vy <- uniformF (-40) (-200)
                let y = 300.0
                return $ Just (x, y, vx, vy)
            else return Nothing

        case ret of
            Just (x, y, vx, vy) -> spawnBeing (makeBeing what (x, y) (vx, vy))
            Nothing -> return ()
