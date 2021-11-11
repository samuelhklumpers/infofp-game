module Animations where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec



data Animation = Implosion Color Vector 
--An implosion is a circle for which the size decreases
--An explosion has increased size 
type TimedAnimation = (Animation, Float)
type TimedAnimations = [TimedAnimation]

implosionspeed = 100

animationStep :: Float -> TimedAnimation -> TimedAnimation
--Tells how the animation changes, and always decreases the time
animationStep dt (ani ,r) = (ani , r - implosionspeed * dt)

drawTimedAnimation :: TimedAnimation -> Picture
drawTimedAnimation ((Implosion c v), r) = color c $ (uncurry translate) v $ circleSolid r

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
