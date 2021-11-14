module Animations where
{- In this module, we handle animations.
 - An animation lasts a couple of frames.
 - We do this by adding a float for the time for any animation, 
 - Each animation step this float decreases and when the float gets under 0, the animation is done.
 - 
 - An Animation instance needs to have a way to draw it at each time, which is handled in drawTimedanimation
 - At the top of the file, we have the animations which are actually used in the game. 
 -
 - The reaper will call the death_to_animation when a being dies. 
 -
 - If you want to add an animation, do it in the Animation datatype
 - all you need to do is specify how to draw it when it has a certain time to live
 -}

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Being
import Control.Lens


data Animation = Implosion Color Vector | Explosion Color Vector Float
--An implosion is a circle for which the size decreases
--An explosion has increased size 

type TimedAnimation = (Animation, Float)
type TimedAnimations = [TimedAnimation]


animationspeed = 100


explode :: Being -> TimedAnimation
explode b =  ((Explosion c p (endR)), (beginR)) where 
                          c = orange -- it turns out this looks cooler
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

drawTimedAnimation :: TimedAnimation -> Picture
drawTimedAnimation ((Implosion c v), r)    = color c $ (uncurry translate) v $ circleSolid r
drawTimedAnimation ((Explosion c v r ), t) = color c $ (uncurry translate) v $ circleSolid (r-t)


animationStep :: Float -> TimedAnimation -> TimedAnimation
animationStep dt (ani ,t) = (ani , t - animationspeed * dt)


cleanAnimations :: TimedAnimations -> TimedAnimations 
cleanAnimations  = filter (\x -> (snd x) > 0)

animationsStep :: Float -> TimedAnimations -> TimedAnimations
animationsStep dt = cleanAnimations.map(animationStep dt ) 


