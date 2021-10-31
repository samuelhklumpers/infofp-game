module World where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Data.Vector.Unboxed.Sized (fromTuple, toList)

import Control.Lens

import Being
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)


data Beings = Beings {player :: Being, asteroids :: [Being], enemies :: [Being], bullets :: [Being]} deriving Show

mapb :: (Being -> Being) -> Beings -> Beings
mapb f (Beings p a e b) = Beings (f p) (map f a) (map f e) (map f b) 


data World = World {beings :: Beings} deriving Show


-- drawing
draw :: World -> Picture
draw w = Pictures $ map drawBeing $ [player b] ++ asteroids b ++ enemies b ++ bullets b
    where b = beings w

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
handler e = id

-- step
step :: Float -> World -> World
step dt =
    damageStep .
    physicsStep dt -- .
    -- etc

-- mark everybody that gets hit, e.g. player hit by bullet -> set damage, asteroid hit by bullet -> exploding, player hit by asteroid -> death animation 
damageStep :: World -> World
damageStep = id

physicsStep :: Float -> World -> World
physicsStep dt =
    collisionStep .
    freeFallStep dt -- .
    -- otherStep .
    -- etc


-- do physics movement, ignoring gravity, collisions, or other sources of acceleration
freeFallStep :: Float -> World -> World
freeFallStep dt w@World {beings = b} = w {beings = mapb (freeFall dt) b}

-- do physics collisions, ignoring death on contact and other effects
collisionStep :: World -> World
collisionStep = id


-- tests
e1 :: R2
e1 = fromTuple (1, 0)

e2 :: R2
e2 = fromTuple (0, 1)

testPlayer :: Being
testPlayer = Being (Phys e1 (0.1 *| e2) 1 0.05) Player

testAsteroid :: Being
testAsteroid = Being (Phys e2 (-0.1 *| e2) 1 0.05) Asteroid

testEnemy :: Being
testEnemy = Being (Phys (e1 + e2) (-0.1 *| e2) 1 0.05) Enemy

testBullet :: Being
testBullet = Being (Phys (0 *| e1) (0.1 *| e1) 1 0.0125) Bullet

testWorld :: World
testWorld = World (Beings testPlayer [testAsteroid] [testEnemy] [testBullet]) 
