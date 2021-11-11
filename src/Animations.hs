module Animations where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec


data Animation = Implosion Color Vector Float 
--An implosion is a circle for which the size decreases

animationStep :: Float -> Animation -> Animation
animationStep dt (Implosion c v r) = Implosion c v (r-dt)

type TimedAnimation = (Animation, Float)
type Animations = [TimedAnimation]

clean :: Animations -> Animations 
clean = undefined -- filter (\x -> snd.x >0)

animationsStep :: Float -> Animations -> Animations
animationsStep dt as = [(animationStep dt a, t-dt )| (a,t)<- as]

drawAnimation :: Animation -> Picture
drawAnimation (Implosion c v r) = color c $ (uncurry translate) v $ circleSolid r
