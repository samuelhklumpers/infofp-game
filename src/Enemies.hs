module Enemies where

import Being
import Shooting
import WorldInit

import Control.Lens
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec


type AI = Being -> World -> Vector


homeAtAI :: AI
homeAtAI b w = w ^. beings . player . phys . pos Vec.- b ^. phys . pos

aimAtAI :: AI
aimAtAI _ w = w ^. beings . player . phys . pos

homeLeadAI :: AI
homeLeadAI = undefined -- move at, take into account player velocity

aimLeadAI :: AI
aimLeadAI = undefined -- same, but for shooting