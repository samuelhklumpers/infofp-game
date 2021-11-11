{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WorldExec where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)
import Data.Functor.Identity
import Control.Lens
import Control.Monad.State
import Control.Monad
import Data.Map (empty, member, Map)
import System.Random
import System.Random.Stateful

import WorldInit
import Being
import Control2
import Shooting
import Util
import Statistics
import Reaping
import Drawing


spawnTick :: Float
spawnTick = 0.1 -- seconds

-- rates as in T ~ Exp(1/t), t in spawnTicks
toRate :: Float -> Float
toRate interval = perTick where
    perSecond = 1 / interval
    perTick = perSecond * spawnTick

handler :: Event -> World -> World
handler = Control2.handleInput

step :: Float -> World -> IO World
step dt = execStateT $ do
    gs <- use gameState

    case gs of
        Playing -> do
            modify $ spawnStep dt
            modify $ fireStep dt
            modify $ userStep dt
            modify $ physicsStep dt
            modify damageStep
            modify $ scoreStep dt
        PlayerDied -> gameEndStep
        _ -> return ()


gameEndStep :: StateT World IO ()
gameEndStep = do
    w <- get

    let newScores = _stats w:w ^. highscores
    lift $ jdump newScores "scores.json"

    gameState .= GameOver


uniformF :: Float -> Float -> State StdGen Float
uniformF l h = state $ uniformR (l, h)


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

physicsStep :: Float -> World -> World
physicsStep dt =
    collisionStep .
    freeFallStep dt


scoreStep :: Float -> World -> World
scoreStep dt = stats . survived +~ Identity dt


playerAccel :: Float
playerAccel = 8

toggleAccel :: Vector -> Bool -> Vector -> Vector
toggleAccel v b w
    | b         = v Vec.+ w
    | otherwise = w

getAccel :: MotionControl -> Vector
getAccel (MotionControl u r d l) =
    toggleAccel e2 u $
    toggleAccel e1 r $
    toggleAccel (Vec.negate e2) d $
    toggleAccel (Vec.negate e1) l v0


userStep :: Float -> World -> World
userStep dt = execState $ do
    a <- uses (userIn . moving) getAccel
    beings . player . phys . vel %= (Vec.+ playerAccel `mulSV` a)


-- do physics movement, ignoring gravity, collisions, or other sources of acceleration
freeFallStep :: Float -> World -> World
freeFallStep dt w@World {_beings = b} = w {_beings = fmap (freeFall dt) b}


-- do physics collisions, ignoring death on contact and other effects
collisionStep :: World -> World
collisionStep = beings %~ collisions


-- tests
testPlayer :: Being
testPlayer = makeBeing (Player 0) v0 v0


testWorld :: Frame -> IO World
testWorld frame = do
    rng <- newStdGen

    return $ World frame (Pointed testPlayer []) blankInput undefined (SpawnData 0 (toRate 0.5) 0) rng Playing [] []
