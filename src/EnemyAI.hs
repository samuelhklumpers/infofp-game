module EnemyAI where


import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens

import Being
import Util


homeAtAI :: MoveAI
homeAtAI self others = others ^. player . phys . pos Vec.- self ^. phys . pos

floatAI :: MoveAI
floatAI self others = v0

aimAtAI :: AimAI
aimAtAI _ others = Just $ others ^. player . phys . pos
