module Control2 where

import WorldInit
import Shooting
import Being
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State

handleInput :: Event -> World -> World
handleInput (EventKey (MouseButton LeftButton) _ _ mousepos) = shoot mousepos
handleInput (EventKey (Char 'p') _ _ _) = pause
handleInput (EventKey (Char 'w') _ _ _) = controlU
handleInput (EventKey (Char 'a') _ _ _) = controlL
handleInput (EventKey (Char 's') _ _ _) = controlD
handleInput (EventKey (Char 'd') _ _ _) = controlR
handleInput _ = id

pause :: World -> World
pause = execState $ do paused %= not 

controlPlayer :: Vector -> World -> World
controlPlayer v = do beings.player.phys.vel %~ (Vec.+ v)

controlU = controlPlayer (0,1)
controlD = controlPlayer (0,-1)
controlL = controlPlayer (-1,0)
controlR = controlPlayer (1,0)

shoot :: Vector -> World -> World
shoot target  = execState $ do 
	p <- use (beings.player)
	spawnBeing (shootBullet p  target)
