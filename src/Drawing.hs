module Drawing where

import WorldInit
import Being
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import Animations
import Control.Monad.State
import Control.Lens
import Statistics
{-
 - This module handles everything that needs to be drawn in the world, 
 - we draw the beings and the animations in a normal step
 - and when the game is over our paused we draw that as well
 - There are sadly some magic numbers here which might need changing if the framesize changes
 -}

writeScreen :: String -> Picture -- some random numbers, gloss isn't too transparent on this stuff
writeScreen str = scale 0.12 0.12 $ translate (-400) (-200) $ Color white $ Text str

writeList :: [String] -> Picture
writeList = foldr writeAndShift Blank

writeAndShift :: String -> Picture -> Picture
writeAndShift str old = Pictures [writeScreen str, translate 0 (-40) old]

draw :: World -> Picture
draw w = Pictures $ flip execState [] $ do
        let b = Being.toList $ _beings w
        let drawScore = scale 0.12 0.12 $ translate (-3000) (-3000) $ Color white $ Text $ show $w ^. stats

        put $ map drawBeing b  ++ map drawTimedAnimation (_timedAnimations w) ++ [drawScore]

        case w ^. gameState of
            Pausing ->    modify (writeScreen "Paused":)
            GameOver ->   do
                let scores = w ^. highscores
                modify (writeList (map show scores):)
            _ ->          return ()

drawBeing :: Being -> Picture
drawBeing Being {_phys = phys, _race = race}
    = color c $ translate x y $ circleSolid r
    where
        Phys {_pos = p, _radius = r} = phys
        (x, y) = p
        c = colorBeing race
