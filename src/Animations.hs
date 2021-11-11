module Animations where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Picture
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec



data Animation = undefined

animationStep :: Animation -> Animation


type TimedAnimation = (Animation, Int)
type Animations = [TimedAnimation]

clean :: Animations -> Animations 
clean = filter (\x -> snd.x >0)

animate :: Animations -> Animations
animate as = [(animationStep x, t-1)| (x,t)<- as]
