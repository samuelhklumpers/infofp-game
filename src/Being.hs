{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}



module Being where


import qualified Data.Vector.Unboxed.Sized as VS
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

data Phys = Phys {_pos :: R2, _vel :: R2, _mass :: Mass, _radius :: Radius} deriving Show
makeLenses ''Phys

data Race = Player Timeout | Asteroid | Bullet | Enemy Timeout deriving Show

-- undo Race, make GADT? --> type guarantee we don't treat a player as an asteroid
data Being = Being {_phys :: Phys, _race :: Race} deriving Show
makeLenses ''Being


data Beings = Beings {_player :: Being, _asteroids :: [Being], _enemies :: [Being], _bullets :: [Being]} deriving Show
makeLenses ''Beings

-- at this point, I already had found out that making Beings anything other than type Beings = [Being] or Set Being was a mistake
toListB :: Beings -> [Being]
toListB (Beings p as es bs) = [p] ++ as ++ es ++ bs

-- yup
fromListB :: [Being] -> Beings
fromListB = foldr marker b0 where
    marker b bs = case _race b of
        Asteroid    -> asteroids %~ (b:) $ bs
        Player _    -> player .~ b $ bs
        Enemy  _    -> enemies %~ (b:) $ bs
        Bullet      -> bullets %~ (b:) $ bs
    b0 = Beings undefined [] [] []



-- move assuming no gravity or collision
freeFall :: Float -> Being -> Being
freeFall dt = phys %~ freeFall' dt

freeFall' :: Float -> Phys -> Phys
freeFall' dt p = pos +~ dt *| (p ^. vel) $ p

-- Just (collision vector) when colliding, otherwise None
-- TODO need posteriori collision detection to prevent _bullets teleporting through thin surfaces
collide :: Being -> Being -> Maybe R2
collide a b
    | d1 <= d2  = Just v
    | otherwise = Nothing where
        v = (a ^. phys . pos) - (b ^. phys . pos)
        d1 = norm v
        d2 = (a ^. phys . radius) + (b ^. phys . radius)


-- elastic collision
collisions :: Beings -> Beings
collisions = fromListB . doCollisions . toListB


-- to lazy to do actual sphere-sphere collision so put point-point for now
bump :: Being -> Being -> (Being, Being)
bump a b = case cv of 
        Just _  -> (phys . vel .~ v1 $ a, phys . vel .~ v2 $ b)
        Nothing -> (a, b) 
    where
        cv = collide a b

        u1 = a ^. phys . vel
        u2 = b ^. phys . vel
        m1 = a ^. phys . mass
        m2 = b ^. phys . mass

        -- ideally *| has higher fixity than +-, but lower than */, but that doesn't seem possible...
        v1 = ((m1 - m2) / (m1 + m2)) *| u1 +   (2 * m2 / (m1 + m2)) *| u2
        v2 =   (2 * m1 / (m1 + m2)) *| u1  + ((m2 - m1) / (m1 + m2)) *| u2


-- 8)
doCollisions :: [Being] -> [Being]
doCollisions bs = runST $ do
    let n = length bs

    arr <- newListArray (1, n) bs :: ST s (STArray s Int Being)

    forM_ [1..n] $ \i ->
        forM_ [i+1..n] $ \j -> do
            a <- readArray arr i
            b <- readArray arr j
            let (a', b') = bump a b
            writeArray arr i a'
            writeArray arr j b'

    getElems arr