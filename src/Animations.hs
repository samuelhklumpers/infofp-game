module Animations where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Being
import Control.Lens


explode :: Being -> TimedAnimation
explode b =  ((Explosion c p (endR)), (beginR)) where 
                          c = orange--colorBeing(b^.race)
                          p = (b^.phys.pos)
                          beginR = (b^.phys.radius)
                          endR   = 2 * beginR
                        
implode :: Being -> TimedAnimation
implode b =  ((Implosion c p ), r) where 
                          c = colorBeing(b^.race)
                          p = (b^.phys.pos)
                          r = (b^.phys.radius)
                            
death_to_animation :: Being -> TimedAnimation
death_to_animation b = case (b^.race) of 
                         Bullet -> explode b 
                         _      -> implode b


data Animation = Implosion Color Vector | Explosion Color Vector Float
--An implosion is a circle for which the size decreases
--An explosion has increased size 
--




type TimedAnimation = (Animation, Float)
type TimedAnimations = [TimedAnimation]

animationspeed = 100

animationStep :: Float -> TimedAnimation -> TimedAnimation
--Tells how the animation changes, and always decreases the time
animationStep dt (ani ,t) = (ani , t - animationspeed * dt)

drawTimedAnimation :: TimedAnimation -> Picture
drawTimedAnimation ((Implosion c v), r)    = color c $ (uncurry translate) v $ circleSolid r
drawTimedAnimation ((Explosion c v r ), t) = color c $ (uncurry translate) v $ circleSolid (r-t)

--drawTimedAnimation ::TimedAnimation -> Picture
--drawTimedAnimation = drawAnimation.fst

cleanAnimations :: TimedAnimations -> TimedAnimations 
cleanAnimations  = filter (\x -> (snd x) > 0)

animationsStep :: Float -> TimedAnimations -> TimedAnimations
animationsStep dt = cleanAnimations.map(animationStep dt ) 


{-
data Animation = Implosion Color Vector Float 
--An implosion is a circle for which the size decreases

implosionspeed = 100

animationStep :: Float -> Animation -> Animation
animationStep dt (Implosion c v r) = Implosion c v (r-implosionspeed * dt)

drawAnimation :: Animation -> Picture
drawAnimation (Implosion c v r) = color c $ (uncurry translate) v $ circleSolid r

drawTimedAnimation ::TimedAnimation -> Picture
drawTimedAnimation = drawAnimation.fst

type TimedAnimation = (Animation, Float)
type TimedAnimations = [TimedAnimation]

cleanAnimations :: TimedAnimations -> TimedAnimations 
cleanAnimations  = filter (\x -> (snd x) > 0)

animationsStep :: Float -> TimedAnimations -> TimedAnimations
animationsStep dt as = cleanAnimations [(animationStep dt a, t-dt )| (a,t)<- as]
-}
