{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}



module Being where


--import qualified Data.Vector.Unboxed.Sized as VS
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec

import Control.Lens
import GHC.TypeNats (KnownNat)
import Data.Array.MArray
import Control.Monad

import Util
import Control.Monad.ST
import Data.Array.ST


type Mass = Float
type Radius = Float
type Timeout = Float
type Health = Int
type TimeSinceLastShot = Float


data Phys = Phys {_pos :: Vector, _vel :: Vector, _mass :: Mass, _radius :: Radius} deriving Show
makeLenses ''Phys


data Race = Player Timeout | Asteroid | Bullet | Enemy Timeout deriving Show

-- undo Race, make GADT? --> type guarantee we don't treat a player as an asteroid
data Being = Being {_phys :: Phys, _race :: Race, _health :: Health, _timeSinceLastShot :: TimeSinceLastShot} deriving Show
makeLenses ''Being


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


--data Beings = Beings {_player :: Being, _asteroids :: [Being], _enemies :: [Being], _bullets :: [Being]} deriving Show
--makeLenses ''Beings
type Beings = Pointed Being

-- at this point, I already had found out that making Beings anything other than type Beings = [Being] or Set Being was a mistake
--toListB :: Beings -> [Being]
--toListB (Beings p as es bs) = [p] ++ as ++ es ++ bs

-- yup
{-fromListB :: [Being] -> Beings
fromListB = foldr marker b0 where
    marker b bs = case _race b of
        Asteroid {} -> asteroids %~ (b:) $ bs
        Player {}   -> player .~ b $ bs
        Enemy  {}   -> enemies %~ (b:) $ bs
        Bullet {}   -> bullets %~ (b:) $ bs
    b0 = Beings (makeBeing (Player 0) v0 v0) [] [] []
-}


-- move assuming no gravity or collision
freeFall :: Float -> Being -> Being
freeFall dt = phys %~ freeFall' dt

freeFall' :: Float -> Phys -> Phys
freeFall' dt p = pos %~ (Vec.+ mulSV dt (p ^.vel)) $ p

-- Just (collision vector) when colliding, otherwise None
-- TODO need posteriori collision detection to prevent _bullets teleporting through thin surfaces
collide :: Being -> Being -> Maybe Vector
collide a b
    | d1 <= d2  = Just v
    | otherwise = Nothing where
        v = (a ^. phys . pos) Vec.- (b ^. phys . pos)
        d1 = magV v
        d2 = (a ^. phys . radius) + (b ^. phys . radius)


-- elastic collision
collisions :: Beings -> Beings
collisions = fromList . doCollisions . toList


-- to lazy to do actual sphere-sphere collision so put point-point for now
bump :: Being -> Being -> (Being, Being)
bump a b = (phys . vel .~ v1 $ a, phys . vel .~ v2 $ b)
    where
        cv = collide a b

        u1 = a ^. phys . vel
        u2 = b ^. phys . vel
        m1 = a ^. phys . mass
        m2 = b ^. phys . mass

        -- ideally *| has higher fixity than +-, but lower than */, but that doesn't seem possible...
        v1 = mulSV ((m1 - m2) / (m1 + m2)) u1 Vec.+ mulSV (2 * m2 / (m1 + m2)) u2
        v2 = mulSV (2 * m1 / (m1 + m2)) u1 Vec.+ mulSV ((m2 - m1) / (m1 + m2)) u2


harm :: Being -> Being -> (Being, Being)
harm a b = case a ^. race of
    Player {} -> case b ^. race of
        Player {} -> error "this game was supposed to be single-player..."
        _         -> (health -~ 1 $ a, health -~ 1 $ b)
    Bullet {} -> case b ^. race of
        --Bullet {} -> (a, b)
        _         -> (health -~ 1 $ a, health -~ 1 $ b)
    Asteroid {} -> case b ^. race of
        Player {} -> harm b a
        Bullet {} -> harm b a
        _         -> (a, b)
    Enemy {} -> case b ^. race of
        Enemy {}  -> (a, b)
        _         -> harm b a


-- 8)
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


makeBeing :: Race -> Vector -> Vector -> Being
makeBeing r x v = case r of
    Player t    -> Being (Phys x v baseMass 16) r baseHp lastFire
    Enemy t     -> Being (Phys x v baseMass 16) r baseHp lastFire
    Asteroid    -> Being (Phys x v baseMass 24) r baseHp lastFire
    Bullet      -> Being (Phys x v baseMass 8)  r baseHp lastFire
    where
        baseHp = 1
        lastFire = 0
        baseMass = 1.0

