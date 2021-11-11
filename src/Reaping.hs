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

import Being
import Animations
import WorldInit

-- replace with actual screen bounds if necessary
isInBounds :: Frame -> Vector -> Bool
isInBounds f v = let (x1, y1) = v in let (x2, y2) = f in
    -x2 <= x1 && x1 <= x2 && -y2 <= y1 && y1 <= y2

-- mark everybody that gets hit, e.g. _player hit by bullet -> set damage, asteroid hit by bullet -> exploding, _player hit by asteroid -> death animation 
damageStep :: World -> World
damageStep = execState $ do
    bs <- uses beings Being.toList
    fr <- use frame

    let bs'        = filter (isInBounds fr . (^. phys . pos)) bs -- maybe prevent deleting the player
    let bs''       = filter ((>0) . (^. health)) bs'
    let deathlist  = filter ((<=0).(^.health)) bs'
    let corpselist = map death_to_animation deathlist
    timedAnimations <>= (corpselist)

    beings .= Being.fromList bs''

implosioncolor = red
death_to_animation :: Being -> TimedAnimation
death_to_animation b = (Implosion implosioncolor (b^.phys.pos) r, r) where r = b^.phys.radius
