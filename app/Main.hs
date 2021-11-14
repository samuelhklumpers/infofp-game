module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Data.Aeson
import Data.Functor.Identity
import Control.Lens
import Data.Bifunctor
import Data.Maybe ( fromMaybe )
import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Unboxed.Sized as VS
import Control.Exception
import System.Exit

import WorldInit
import WorldExec
import Statistics
import Util
import Drawing
{-
 - This module uses the statistics loader
 - and runs the game as specified in WorldExec
 -}

w :: Int
h :: Int
w = 800
h = 800
--The position of where we draw the scores is based on these numbers
--So if you change these (or find a nice formula)
--also change them in the Drawing module

windowFrame :: (Float, Float)
windowFrame = (fromIntegral w / 2, fromIntegral h / 2)


window :: Display
window = InWindow "Awesome Asteroids Game" (w, h) (10, 10)

background :: Color
background = black

fps :: Int
fps = 30

maximum' :: [Identity Int] -> Identity Int
maximum' [] = 0
maximum' xs = maximum xs

makeStats :: [Stats] -> Stats
makeStats xs = blankStats & attempt .~ maximum' (map _attempt xs) + 1

main :: IO ()
main = do
    scores <- fromMaybe [] <$> (jload "scores.json" :: IO (Maybe [Stats]))

    let stats' = makeStats scores
    world <- testWorld windowFrame stats' scores

    playIO window background fps world (return . draw) ((return .) . handler) step


