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
import Reaping 
import Animations
import Spawning



handler :: Event -> World -> World
handler = Control2.handleInput

{-
handleInput :: Event -> World -> World
handleInput e = execState $ do
    case e of
        e'@(EventKey key s _ _)   -> do
            if member key dirMap then
                keyMap %= flip handleKeyState e'
            else
                when (key == Char 'p' && s == Down) $ paused %= not
        _                       -> return ()
-}

{-
-- step
step :: Float -> World -> World
step dt = execState $ do
    p <- use (userIn . pausing)

    unless p $ do
        modify $ fireStep dt
        modify damageStep
        modify $ physicsStep dt
        modify $ userStep dt
        modify $ scoreStep dt
        modify $ spawnStep dt
        modify $ anistep dt 

anistep :: Float -> World -> World  
anistep dt = execState $ do timedAnimations %= (animationsStep dt)

-}

{-

fireTimeout :: Float
fireTimeout = 0.3


fireStep :: Float -> World -> World


fireStep dt = execState $ do
    r <- use $ beings . player . race

    case r of
        Player t -> do
            let t' = min (t + dt) fireTimeout

            fire <- use (userIn . firing)
            if fire && t' >= fireTimeout then do
                beings . player . race .= Player 0
                ph <- use $ beings . player . phys
                let x = ph ^. pos
                let v = ph ^. vel
                let rad = ph ^. radius
                spawnBeing (makeBeing Bullet (x Vec.+ (2 * rad) `mulSV` e2) (v Vec.+ 200 `mulSV` e2))
            else
                beings . player . race .= Player t'
-}
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
            modify $ anistep dt 
        PlayerDied -> gameEndStep
        _ -> return ()

anistep :: Float -> World -> World  
anistep dt = execState $ do timedAnimations %= (animationsStep dt)

            


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
