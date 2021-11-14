module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Data.Aeson
import Data.Functor.Identity
import Control.Lens
import Data.Bifunctor
import Data.Maybe
import Data.IORef
import qualified Data.Map as M
import qualified Data.Vector.Unboxed.Sized as VS
import Control.Exception
import System.Exit

import Init
import Config
import Rules
import Controls
import Drawing
{-
 - This module runs the game as specified in Rules
 -}


main :: IO ()
main = do
    world <- initWorld

    playIO window background fps world (return . draw) ((return .) . handleInput) step


