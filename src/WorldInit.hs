{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WorldInit where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)
import Data.Functor.Identity
import Control.Lens
import Control.Monad.State
import Data.Map (empty, member)
import System.Random
import System.Random.Stateful


import Being
import Util
import Statistics

type Frame = (Float, Float)

type Firing = Maybe Vector

data SpawnData = SpawnData {_timeSinceLast :: Float, _asteroidRate :: Float, _enemyRate :: Float} deriving Show
makeLenses ''SpawnData

data MotionControl = MotionControl {
    _motionU :: Bool,
    _motionR :: Bool,
    _motionD :: Bool,
    _motionL :: Bool
}
makeLenses ''MotionControl

data UserInput = UserInput {
    _pausing :: Bool,
    _firing  :: Firing,
    _moving  :: MotionControl
}
makeLenses ''UserInput

blankMotion :: MotionControl
blankMotion = MotionControl False False False False

blankInput :: UserInput
blankInput = UserInput False Nothing blankMotion

data World = World {
    _frame :: Frame,
    _beings :: Beings,
    _userIn :: UserInput,
    _stats :: Stats' Identity,
    _spawns :: SpawnData,
    _randomizer :: StdGen,
    highscores :: [Stats' Identity]
}
makeLenses ''World


--glospos2uspos :: World -> Vector -> Vector
--glospos2uspos w (x,y) = (h-x,y) where (h,b) = (w ^. frame)

instance Show World where
    show w = ":(" -- put stuff here StateGenM doesn't print nice



spawnBeing :: Being -> State World ()
spawnBeing b = beings <>= terminal b
