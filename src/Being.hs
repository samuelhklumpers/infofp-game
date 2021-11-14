{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}



module Being
    (module Being,
     module Pointed) where


import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Control.Lens


import Pointed
{-
 - This file describes all the physics, all the beings and how they interact
 - Beings can be divided into races, but a race is not limiting to what a being can do in potetential
 - When you add data to the being don't forget to give each race a standard value for that data
 - And when you add a race don't forget to implement how it interacts with other beings on collisions
 -
 -}

type Mass = Float
type Radius = Float
type Timeout = Float
type Health = Int
type TimeSinceLastShot = Float

data Phys = Phys {_pos :: Vector, _vel :: Vector, _mass :: Mass, _radius :: Radius} deriving (Eq, Show)
makeLenses ''Phys

type Beings = Pointed Being

type AimAI = Being -> Beings -> Maybe Vector
type MoveAI = Being -> Beings -> Vector
data Race = Player | Asteroid | Bullet | Enemy AimAI MoveAI | Chaser

instance Eq Race where
    Player {}   == Player {}   = True
    Asteroid {} == Asteroid {} = True
    Enemy {}    == Enemy {}    = True
    Bullet {}   == Bullet {}   = True
    Chaser {}   == Chaser {}   = True
    _ == _ = False

data Turreted = Turret Timeout | NoTurret deriving (Eq, Show)


data Being = Being {_phys :: Phys, _race :: Race, _health :: Health, _timeSinceLastShot :: TimeSinceLastShot, _turreted :: Turreted} deriving Eq
makeLenses ''Being

-- Below all the starting conditions a Race should have 
-- note these are in principle not fixed troughout the game
-- meaning Turrets can be placed on all beings 

radiusBeing :: Race -> Radius
radiusBeing Player   {} = 16
radiusBeing Enemy    {} = 16
radiusBeing Asteroid {} = 24
radiusBeing Bullet   {} = 8
radiusBeing Chaser   {} = 10

turretBeing :: Race -> Turreted
turretBeing Player   = Turret 0.3
turretBeing Enemy {} = Turret 1.0
turretBeing Asteroid = NoTurret
turretBeing Bullet   = NoTurret
turretBeing Chaser   = NoTurret

colorBeing :: Race -> Color
colorBeing Player {}   = blue
colorBeing Enemy {}    = red
colorBeing Asteroid {} = greyN 0.5
colorBeing Bullet {}   = yellow
colorBeing Chaser {}   = green

scoreBeing :: Race -> Int
scoreBeing Player {} = 0
scoreBeing Enemy {}  = 20
scoreBeing Asteroid  = 10
scoreBeing Bullet    = 0
scoreBeing Chaser    = 15
-- End of Race starting conditions, don't forget the collision harm effects!
-- and don't forget to actually spawn them


makeBeing :: Race -> Vector -> Vector -> Being
makeBeing r x v = Being (Phys x v baseMass (radiusBeing r) ) r baseHP lastFire (turretBeing r) where
                        baseHP = 1
                        lastFire = 0
                        baseMass = 1.0


harm :: Being -> Being -> (Being, Being)
harm a b = case a ^. race of
    Player {} -> case b ^. race of
        Player {} -> error "this game was supposed to be single-player..."
        _         -> (health -~ 1 $ a, health -~ 1 $ b)
    Bullet {} -> (health -~ 1 $ a, health -~ 1 $ b)
    Asteroid {} -> case b ^. race of
        Player {} -> harm b a
        Bullet {} -> harm b a
        _         -> (a, b)
    Enemy {} -> case b ^. race of
        Enemy {}  -> (a, b)
        _         -> harm b a
    Chaser {} -> case b^.race of 
        Chaser {} -> (a,b)
        _         -> harm b a 

