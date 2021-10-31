module Controls where


import Data.Map ( Map, insert, insertWith, fromList, foldrWithKey, findWithDefault )
import qualified Data.Map as M

import Graphics.Gloss.Interface.Pure.Game
import Control.Lens


import Util


type KeyMap = Map Key KeyState
emptyKM :: KeyMap
emptyKM = M.empty


handleKeyState :: KeyMap -> Event -> KeyMap
handleKeyState km (EventKey key s mod _) = insert key s km 
handleKeyState _ _ = error "handleKeyState got EventMotion or EventResize"

setDefault :: Ord k => k -> v -> Map k v -> Map k v
setDefault = insertWith (\_ x -> x)



-- make rebindable from some json
controlU = Char 'i'
controlR = Char 'l'
controlD = Char 'k'
controlL = Char 'j'
controlFire = Char 'f'


dirMap :: Map Key R2
dirMap = fromList [
        (controlU, e2),
        (controlR, e1),
        (controlD, -e2),
        (controlL, -e1)
    ]


getAccel :: KeyMap -> R2
getAccel km = foldrWithKey f v0 dirMap where
    f k v v' = case M.lookup k km of
        Just Down -> v + v'
        _       -> v'

