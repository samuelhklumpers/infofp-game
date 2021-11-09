module Shooting where 

import Util
import Being

data Firing = NoShots | Shot R2

processfire :: Event -> Firing
processfire (EventKey (MouseButon LeftButton ) _ mousepos) = Shot (fromTuple mousepos)
processfire _ = NoShots


shootBullet :: Being -> R2 -> Being
shootBullet shooter targetpos = makeBeing Bullet startpos velocity where
                                 shooterpos = shooter ^. phys. pos
                                 direction  = unit (targetpos - shooterpos))
                                 bulletspeed= 22
                                 velocity   = bulletspeed *| direction
                                 startpos   = shooterpos  + ((shooter ^. phys.radius) *| direction )
                                
