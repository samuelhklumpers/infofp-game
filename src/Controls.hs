module Controls where


import Data.Map ( Map, insert, insertWith, fromList, foldrWithKey, findWithDefault )
import qualified Data.Map as M

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens


import Util
import Shooting

type KeyMap = Map Key KeyState
emptyKM :: KeyMap
emptyKM = M.empty

--Event data constructors: 
--EventKey Key KeyState Modifiers (Float, Float)	 
--EventMotion (Float,Float)


handleKeyState :: KeyMap -> Event -> KeyMap
handleKeyState km (EventKey key s _ _) = insert key s km
handleKeyState _ _ = error "handleKeyState got EventMotion or EventResize"

setDefault :: Ord k => k -> v -> Map k v -> Map k v
setDefault = insertWith (\_ x -> x)



-- make rebindable from some json
controlU = Char 'i'
controlR = Char 'l'
controlD = Char 'k'
controlL = Char 'j'
controlFire = Char 'f'


dirMap :: Map Key Vector
dirMap = fromList [
        (controlU, e2),
        (controlR, e1),
        (controlD, Vec.negate e2),
        (controlL, Vec.negate e1)
    ]


getAccel :: KeyMap -> Vector
getAccel km = foldrWithKey f v0 dirMap where
    f k v v' = case M.lookup k km of
        Just Down -> v Vec.+ v'
        _         -> v'

getFire :: KeyMap -> Bool
getFire km = Just Down == M.lookup controlFire km
