module Shooting where 

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Interact
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens

import Being

data Firing = NoShots | Shot Vector

{-
processfire :: Event -> Firing
processfire (EventKey (MouseButton LeftButton ) _ _ mousepos) = Shot mousepos
processfire _ = NoShots
-}

{-
shootBullet :: Being -> Vector -> Being
shootBullet shooter targetpos = makeBeing Bullet startpos velocity where
                                 shooterpos = shooter ^. phys. pos
                                 direction  = normalizeV (targetpos Vec.- shooterpos)
                                 bulletspeed= 22
                                 velocity   = bulletspeed `mulSV` direction
                                 startpos   = shooterpos  Vec.+ ((shooter ^. phys.radius) `mulSV` direction )
                                
-}



shootBullet :: Being -> Vector -> Maybe Being
shootBullet shooter targetpos  | time < 0.3 = Nothing
                               | otherwise  = Just(makeBeing Bullet startpos velocity) where
                                   time = shooter ^. timeSinceLastShot
                                   shooterpos = shooter ^. phys. pos
                                   direction  = normalizeV (targetpos Vec.- shooterpos)
                                   bulletspeed= 22
                                   velocity   = bulletspeed `mulSV` direction
                                   startpos   = shooterpos  Vec.+ ((shooter ^. phys.radius) `mulSV` direction )
                                                                
