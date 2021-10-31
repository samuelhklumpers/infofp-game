module World where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Being


data Beings = Beings {player :: Being, asteroids :: [Being], enemies :: [Being], bullets :: [Being]} deriving Show

mapb :: (Being -> Being) -> Beings -> Beings
mapb f (Beings p a e b) = Beings (f p) (map f a) (map f e) (map f b) 


data World = World {beings :: Beings} deriving Show


-- drawing
draw :: World -> Picture
draw = undefined

-- handlers
handler :: Event -> World -> World
handler = undefined

-- step
step :: Float -> World -> World
step =
    physicsStep dt -- .
    -- etc

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
testWorld = World (Beings undefined [] [] []) 
