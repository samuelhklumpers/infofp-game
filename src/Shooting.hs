module Shooting where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Interact
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State

import Being
import WorldInit

shoottimeout :: TimeSinceLastShot
shoottimeout = 0.3

bulletspeed :: Float
bulletspeed  = 300

startPosMult :: Float
startPosMult = 1.5


shootBullet :: Being -> Vector -> Maybe Being
--In principe kan alles schieten, leuk als je enemies maakt die turrets kunnen plaatsen op asteroids zodat asteroids op de player schieten. 
shootBullet shooter targetpos  | time < shoottimeout = Nothing
                               | otherwise    = Just (makeBeing Bullet startpos velocity) where
                                   time       = shooter ^. timeSinceLastShot
                                   shooterpos = shooter ^. phys. pos
                                   direction  = normalizeV (targetpos Vec.- shooterpos)
                                   velocity   = bulletspeed `mulSV` direction Vec.+ shooter ^. phys . vel
                                   startpos   = shooterpos  Vec.+ ((startPosMult * (shooter ^. phys.radius)) `mulSV` direction)

playerShot :: World -> Maybe Being
playerShot w = do
    target <- w ^. userIn . firing
    shootBullet (w ^. beings . player) target

fireStep :: Float -> World -> World
fireStep dt = execState $ do
    w <- get
    case playerShot w of
        Nothing     -> beings . player . timeSinceLastShot += dt
        Just bullet -> do
            beings . player . timeSinceLastShot .= 0
            spawnBeing bullet
