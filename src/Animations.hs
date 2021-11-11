module Animations where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec


data Animation = Implosion Color Vector Float 
--An implosion is a circle for which the size decreases

animationStep :: Float -> Animation -> Animation
animationStep dt (Implosion c v r) = Implosion c v (r-dt)

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

