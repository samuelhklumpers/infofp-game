module Reaping where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)
import Data.Functor.Identity
import Control.Lens
import Control.Monad.State
import Data.Map (empty, member, Map)
import System.Random
import System.Random.Stateful
import Data.List
import Statistics
import Being
import Animations
import World

{-
 - this module handles garbage collection and score collection
 -}
isInBounds :: Frame -> Vector -> Bool
isInBounds (x2, y2) (x1, y1) =
    -x2 <= x1 && x1 <= x2 && -y2 <= y1 && y1 <= y2

-- collect all dead and out of bounds beings, and award score
damageStep :: World -> World
damageStep = execState $ do
    player <- use (beings . player)
    bs <- uses beings Being.toList
    fr <- use frame

    let (visible, gone)  = partition (isInBounds fr . (^. phys . pos)) bs
    let (survivors, dead) = partition ( (>0) . (^. health)) visible
    let corpselist       = map deathToAnimation dead

    timedAnimations <>= corpselist

    let f x v = v + scoreBeing (x ^. race)
    stats . score += foldr f 0 dead


    when (player `elem` (dead ++ gone)) $ do
        gameState .= PlayerDied

    beings .= Being.fromList survivors

