module Spawning where

import WorldInit
import Control.Monad.State
import Control.Lens

import Util
import Being


spawnTick :: Float
spawnTick = 0.1 -- seconds

-- rates as in T ~ Exp(1/t), t in spawnTicks
toRate :: Float -> Float
toRate interval = perTick where
    perSecond = 1 / interval
    perTick = perSecond * spawnTick


baseSpawnRates :: SpawnData
baseSpawnRates = SpawnData 0 (toRate 2.0) (toRate 4.0)


spawnStep :: Float -> World -> World
spawnStep dt = execState $ do
    spawns . timeSinceLast += dt
    t <- use (spawns . timeSinceLast)
    rate <- use (spawns . asteroidRate)
    (w, h) <- use frame
    -- put enemy here too

    let n = round (t / spawnTick)
    spawns . timeSinceLast -= fromIntegral n * spawnTick

    forM_ [1..n] $ \_ -> do -- lol
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
            Just (x, y, vx, vy) -> spawnBeing (makeBeing Asteroid (x, y) (vx, vy))
            Nothing -> return ()