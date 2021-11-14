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

turretBeing :: Float -> Race -> Turreted
turretBeing _ Player   = Turret 0.3
turretBeing t Enemy {} = Turret $ 1.0 + (0.5 - 1.0) * t
turretBeing _ Asteroid = NoTurret
turretBeing _ Bullet   = NoTurret
turretBeing _ Chaser   = NoTurret

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

beingHP :: Race -> Int
beingHP Player {}   = 3
beingHP Enemy {}    = 2
beingHP Asteroid {} = 3
beingHP Bullet {}   = 1
beingHP Chaser      = 2

beingMass :: Race -> Float
beingMass Player {}   = 1.0
beingMass Enemy {}    = 1.0
beingMass Asteroid {} = 2.0
beingMass Bullet {}   = 0.1
beingMass Chaser {}   = 0.5
-- End of Race starting conditions, don't forget the collision harm effects!
-- and don't forget to actually spawn them


makeBeing :: Race -> Float -> Vector -> Vector -> Being
makeBeing r t x v = Being (Phys x v (beingMass r) (radiusBeing r)) r (beingHP r) lastFire (turretBeing t r) where
                        lastFire = 0

-- apply damage selectively after collisions
harm :: Being -> Being -> (Being, Being)
harm a b = case a ^. race of
    Player {} -> case b ^. race of
        Player {} -> error "this game was supposed to be single-player..."
        _         -> (health -~ 1 $ a, health -~ 1 $ b)
    Bullet {} -> (health -~ 1 $ a, health -~ 1 $ b)
    Chaser {} -> (health -~ 1 $ a, health -~ 1 $ b)
    Asteroid {} -> case b ^. race of
        Player {} -> harm b a
        Bullet {} -> harm b a
        Chaser {} -> harm b a
        _         -> (a, b)
    Enemy {} -> case b ^. race of
        Enemy {}  -> (a, b)
        _         -> harm b a

