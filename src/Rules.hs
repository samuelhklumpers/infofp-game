module Rules where


import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Data.Functor.Identity
import Data.List
import Control.Lens
import Control.Monad.State
import Data.Map (empty, member, Map)

import World
import Being
import Shooting
import Config
import Controls
import Statistics
import Physics
import Reaping
import Spawning
import Animations
import Chasing
{-
 - This file is where all steps come together
 - also the initial value of the world is loaded here
 -}


step :: Float -> World -> IO World
step dt = execStateT $ do
    gs <- use gameState

    case gs of
        Playing -> do
            modify $ spawnStep dt
            touhouStep
            reloadStep dt
            modify $ fireStep dt
            modify $ userStep dt
            modify $ aiStep dt
            modify chaseStep
            modify $ physicsStep dt
            modify damageStep
            modify $ scoreStep dt
            modify $ anistep dt
        PlayerDied -> gameEndStep
        _ -> return ()

-- increment difficulty over time
touhouStep :: StateT World IO ()
touhouStep = do
    t <- uses (stats . survived) runIdentity
    touhouFactor .= min (t / 60) 1.0

-- recover time since last shot for everyone
reloadStep :: Float -> StateT World IO ()
reloadStep dt = do
    beings %= fmap (reloadBeing dt)

reloadBeing :: Float -> Being -> Being
reloadBeing dt b = b & timeSinceLastShot +~ dt

-- one-off handler for the PlayerDied state, updates highscores and saves them
gameEndStep :: StateT World IO ()
gameEndStep = do
    w <- get

    let newScores = take highscoreSize $ reverse $ sort $ _stats w:w ^. highscores
    lift $ jdump newScores "scores.json"

    highscores .= newScores
    gameState .= GameOver

-- perform all physics, such as collisions and freefall motion
physicsStep :: Float -> World -> World
physicsStep dt =
    collisionStep .
    freeFallStep dt

-- increment time survived
scoreStep :: Float -> World -> World
scoreStep dt = stats . survived +~ Identity dt

-- accelerate the player according to their inputs
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

-- update all ongoing animations
anistep :: Float -> World -> World
anistep dt = execState $ do timedAnimations %= animationsStep dt
