module Drawing where

import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Monad.State
import Control.Lens

import World
import Being
import Animations
import Config

{-
 - This module handles everything that needs to be drawn in the world, 
 - we draw the beings and the animations in a normal step
 - and when the game is over our paused we draw that as well
 - There are sadly some magic numbers here which might need changing if the framesize changes
 -}

textScale :: Float
textScale = 0.12

textHeight :: Float
textHeight = 340 * textScale

writeScreen :: String -> Picture -- some random numbers, gloss isn't too transparent on this stuff
writeScreen str = translate (-fromIntegral windowWidth / 4) (fromIntegral windowHeight / 4) $ scale textScale textScale $ Color white $ Text str

writeList :: [String] -> Picture
writeList = foldr writeAndShift Blank

writeAndShift :: String -> Picture -> Picture
writeAndShift str old = Pictures [writeScreen str, translate 0 (-textHeight) old]

draw :: World -> Picture
draw w = Pictures $ flip execState [] $ do
        let b = Being.toList $ _beings w

        let drawScore = translate (-3 * fromIntegral windowWidth / 8) (-3 * fromIntegral windowHeight / 8) $ scale textScale textScale $ Color white $ Text $ show $ w ^. stats

        put $ map drawBeing b  ++ map drawTimedAnimation (_timedAnimations w) ++ [drawScore]

        case w ^. gameState of
            Pausing ->    modify (writeScreen "Paused":)
            GameOver ->   do
                let scores = w ^. highscores
                modify (writeList (map show scores):)
            _ ->          return ()

drawBeing :: Being -> Picture
drawBeing Being {_phys = phys, _race = race} = color c $ translate x y $ circleSolid r where
    Phys {_pos = p, _radius = r} = phys
    (x, y) = p
    c = colorBeing race
