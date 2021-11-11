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
import System.Random.Stateful

import WorldInit
import Being
import Control2
import Shooting
import Util
import Statistics
import Reaping
import Spawning
import Drawing



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


testWorld :: Frame -> Stats -> [Stats] -> IO World
testWorld frame stats' scores = do
    rng <- newStdGen

    return $ World
        frame
        (Pointed testPlayer [])
        blankInput
        stats'
        baseSpawnRates
        rng Playing
        scores
        []
