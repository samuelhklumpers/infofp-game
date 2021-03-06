{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module World where

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
import Config
import Statistics
import Animations

type Frame = (Float, Float)

type Firing = Maybe Vector

data SpawnData = SpawnData {_timeSinceLast :: Float, _asteroidRate :: Float, _enemyRate :: Float, _chaserRate ::Float} deriving Show
makeLenses ''SpawnData

data MotionControl = MotionControl {
    _motionU :: Bool,
    _motionR :: Bool,
    _motionD :: Bool,
    _motionL :: Bool
}
makeLenses ''MotionControl

data UserInput = UserInput {
    _firing  :: Firing,
    _moving  :: MotionControl
}
makeLenses ''UserInput

blankMotion :: MotionControl
blankMotion = MotionControl False False False False

blankInput :: UserInput
blankInput = UserInput Nothing blankMotion

data GameState = Playing | Pausing | PlayerDied | GameOver | Exiting deriving Eq

data World = World {
    _frame :: Frame,
    _beings :: Beings,
    _userIn :: UserInput,
    _stats :: Stats' Identity,
    _spawns :: SpawnData,
    _randomizer :: StdGen,
    _gameState :: GameState,
    _touhouFactor :: Float,
    _highscores :: [Stats' Identity],
    _timedAnimations :: TimedAnimations
}
makeLenses ''World

instance Show World where
    show w = ":(" -- put stuff here StateGenM doesn't print nice

spawnBeing :: Being -> State World ()
spawnBeing b = beings <>= terminal b


