{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}



module Being where


import Data.Vector.Unboxed.Sized as VS

import Control.Lens
import GHC.TypeNats (KnownNat)


type R2 = Vector 2 Float

data Phys = Phys {_pos :: R2, _vel :: R2, _mass :: Float, _radius :: Float} deriving Show
makeLenses ''Phys

data Race = Player | Asteroid | Bullet | Enemy deriving Show

-- undo Race, make GADT? --> type guarantee we don't treat a player as an asteroid
data Being = Being {_phys :: Phys, _race :: Race} deriving Show
makeLenses ''Being


-- move these to new module
(*|) :: (Unbox a, Num a, KnownNat d) => a -> Vector d a -> Vector d a
m *| v = VS.map (*m) v

-- move these to new module
norm :: (Unbox a, Floating a, KnownNat d) => Vector d a -> a
norm = sqrt . VS.foldr ((+) . (^2)) 0

-- move assuming no gravity or collision
freeFall :: Float -> Being -> Being
freeFall dt = phys %~ freeFall' dt

freeFall' :: Float -> Phys -> Phys
freeFall' dt p = pos +~ dt *| (p ^. vel) $ p 

-- Just (collision vector) when colliding, otherwise None
-- TODO need posteriori collision detection to prevent bullets teleporting through thin surfaces
collide :: Being -> Being -> Maybe R2
collide a b
    | d1 <= d2  = Just v
    | otherwise = Nothing where
        v = (a ^. phys . pos) - (b ^. phys . pos)
        d1 = norm v
        d2 = (a ^. phys . radius) + (b ^. phys . radius)
          
