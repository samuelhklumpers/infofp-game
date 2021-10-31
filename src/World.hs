{-# LANGUAGE TemplateHaskell #-}

module World where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Data.Vector.Unboxed.Sized (fromTuple, toList)
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)
import Data.Functor.Identity
import Control.Lens
import Data.Map (empty)

import Being
import Controls
import Util
import Statistics


mapb :: (Being -> Being) -> Beings -> Beings
mapb f (Beings p a e b) = Beings (f p) (map f a) (map f e) (map f b)


data World = World {_beings :: Beings, _keyMap :: KeyMap, _stats:: Stats' Identity, highscores :: [Stats' Identity]} deriving Show
makeLenses ''World


-- drawing
draw :: World -> Picture
draw w = Pictures $ map drawBeing $ [_player b] ++ _asteroids b ++ _enemies b ++ _bullets b
    where b = _beings w

drawBeing :: Being -> Picture
drawBeing Being {_phys = phys, _race = race}
    = color c $ translate x y $ circleSolid r
    where
        Phys {_pos = p, _radius = r} = phys
        [x, y] = toList p
        c = colorBeing race

colorBeing :: Race -> Color
colorBeing race = case race of
    Player      -> blue
    Enemy       -> red
    Asteroid    -> greyN 0.5
    Bullet      -> yellow

-- handlers
handler :: Event -> World -> World
handler e w = case e of
    e'@EventKey {} -> keyMap %~ flip handleKeyState e' $ w
    _              -> w

-- step
step :: Float -> World -> World
step dt =
    damageStep .
    physicsStep dt .
    userStep dt .
    scoreStep dt

-- mark everybody that gets hit, e.g. _player hit by bullet -> set damage, asteroid hit by bullet -> exploding, _player hit by asteroid -> death animation 
damageStep :: World -> World
damageStep = id

physicsStep :: Float -> World -> World
physicsStep dt =
    collisionStep .
    freeFallStep dt -- .
    -- otherStep .
    -- etc

scoreStep :: Float -> World -> World
scoreStep dt = stats . survived +~ Identity dt -- I hate Identity


playerAccel :: Float
playerAccel = 0.02

-- put some upper limit on vel?

userStep :: Float -> World -> World
userStep dt w = beings . player . phys . vel +~ playerAccel *| a $ w
    where a = getAccel (w ^. keyMap)


-- do physics movement, ignoring gravity, collisions, or other sources of acceleration
freeFallStep :: Float -> World -> World
freeFallStep dt w@World {_beings = b} = w {_beings = mapb (freeFall dt) b}

-- do physics collisions, ignoring death on contact and other effects
collisionStep :: World -> World
collisionStep = beings %~ collisions


-- tests
testPlayer :: Being
testPlayer = Being (Phys (0.25 *| e1 + 0.25 *| e2) v0 1 0.05) Player

testAsteroid :: Being
testAsteroid = Being (Phys e2 v0 1 0.05) Asteroid

testEnemy :: Being
testEnemy = Being (Phys (e1 + e2) v0 1 0.05) Enemy

testBullet :: Being
testBullet = Being (Phys (0 *| e1) v0 1 0.0125) Bullet


testWorld :: World
testWorld = World (Beings testPlayer [testAsteroid] [testEnemy] [testBullet]) emptyKM (Stats' 0.0) []