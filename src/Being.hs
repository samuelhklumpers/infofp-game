{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}



module Being where


--import qualified Data.Vector.Unboxed.Sized as VS
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture


import Control.Lens
import GHC.TypeNats (KnownNat)
import Data.Array.MArray
import Control.Monad

import Util
import Control.Monad.ST
import Data.Array.ST
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


data Pointed a = Pointed {_player :: a, _npo :: [a]}
makeLenses ''Pointed

toList :: Pointed a -> [a]
toList (Pointed x xs) = x:xs

fromList :: [a] -> Pointed a
fromList (x:xs) = Pointed x xs
fromList _ = error "empty list cannot be Pointed"

instance Functor Pointed where
    fmap f = fromList . fmap f . toList

instance Semigroup (Pointed a) where
    p <> q = fromList $ toList p <> toList q

deriving instance Show a => Show (Pointed a)

terminal :: a -> Pointed a
terminal x = Pointed x []

type Beings = Pointed Being


type AimAI = Being -> Beings -> Maybe Vector
type MoveAI = Being -> Beings -> Vector
data Race = Player | Asteroid | Bullet | Enemy AimAI MoveAI

instance Eq Race where
    Player {} == Player {} = True
    Asteroid {} == Asteroid {} = True
    Enemy {} == Enemy {} = True
    Bullet {} == Bullet {} = True
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

turretBeing :: Race -> Turreted
turretBeing Player   = Turret 0.3
turretBeing Enemy {} = Turret 1.0
turretBeing Asteroid = NoTurret
turretBeing Bullet   = NoTurret

colorBeing :: Race -> Color
colorBeing Player {}   = blue
colorBeing Enemy {}    = red
colorBeing Asteroid {} = greyN 0.5
colorBeing Bullet {}   = yellow

scoreBeing :: Race -> Int
scoreBeing Player {} = 0
scoreBeing Enemy {}  = 10
scoreBeing Asteroid  = 10
scoreBeing Bullet    = 0
-- End of Race starting conditions, don't forget the collision harm effects!

canShoot :: Being -> Bool 
canShoot b = case (b^.turreted) of 
                NoTurret -> False 
                Turret timeout -> timeout < (b^.timeSinceLastShot)

makeBeing :: Race -> Vector -> Vector -> Being
makeBeing r x v = Being (Phys x v baseMass (radiusBeing r) ) r baseHP lastFire (turretBeing r) where
                        baseHP = 1
                        lastFire = 0
                        baseMass = 1.0




-- move assuming no gravity or collision
freeFall :: Float -> Being -> Being
freeFall dt = phys %~ freeFall' dt

freeFall' :: Float -> Phys -> Phys
freeFall' dt p = pos %~ (Vec.+ mulSV dt (p ^.vel)) $ p

collide :: Being -> Being -> Maybe Vector
--Inputs 2 beings, outputs the position of their collision if there's a collision, nothing if there's no collission. 
collide a b
    | d1 <= d2  = Just v
    | otherwise = Nothing where
        v = (a ^. phys . pos) Vec.- (b ^. phys . pos)
        d1 = magV v
        d2 = (a ^. phys . radius) + (b ^. phys . radius)


-- elastic collision
collisions :: Beings -> Beings
collisions = fromList . doCollisions . toList


-- It is sufficient to model a bump as a collision of 2 point particles
bump :: Being -> Being -> (Being, Being)
bump a b = (phys . vel .~ v1 $ a, phys . vel .~ v2 $ b)
    where
        cv = collide a b

        u1 = a ^. phys . vel
        u2 = b ^. phys . vel
        m1 = a ^. phys . mass
        m2 = b ^. phys . mass

        v1 = mulSV ((m1 - m2) / (m1 + m2)) u1 Vec.+ mulSV (2 * m2 / (m1 + m2)) u2
        v2 = mulSV (2 * m1 / (m1 + m2)) u1 Vec.+ mulSV ((m2 - m1) / (m1 + m2)) u2


harm :: Being -> Being -> (Being, Being)
harm a b = case a ^. race of
    Player {} -> case b ^. race of
        Player {} -> error "this game was supposed to be single-player..."
        _         -> (health -~ 1 $ a, health -~ 1 $ b)
    Bullet {} -> case b ^. race of
        --Bullet {} -> (a, b) 
        --If you want bullets to bounce turn this comment of
        _         -> (health -~ 1 $ a, health -~ 1 $ b)
    Asteroid {} -> case b ^. race of
        Player {} -> harm b a
        Bullet {} -> harm b a
        _         -> (a, b)
    Enemy {} -> case b ^. race of
        Enemy {}  -> (a, b)
        _         -> harm b a


doCollisions :: [Being] -> [Being]
doCollisions bs = runST $ do
    let n = length bs

    arr <- newListArray (1, n) bs :: ST s (STArray s Int Being)

    forM_ [1..n] $ \i ->
        forM_ [i+1..n] $ \j -> do
            a <- readArray arr i
            b <- readArray arr j

            let cv = collide a b

            case cv of
                Just _  -> do
                    let (a', b') = bump a b
                    let (a'', b'') = harm a' b'

                    writeArray arr i a''
                    writeArray arr j b''

                _       -> return ()

    getElems arr


homeAtAI :: MoveAI
homeAtAI self others = others ^. player . phys . pos Vec.- self ^. phys . pos

floatAI :: MoveAI
floatAI self others = v0

aimAtAI :: AimAI
aimAtAI _ others = Just $ others ^. player . phys . pos

