{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}


module Util where


import Data.Vector.Unboxed.Sized as VS
import Control.Lens
import GHC.TypeNats (KnownNat)


type R2 = Vector 2 Float


-- move these to new module
(*|) :: (Unbox a, Num a, KnownNat d) => a -> Vector d a -> Vector d a
m *| v = VS.map (*m) v
infixl 8 *|


-- move these to new module
norm :: (Unbox a, Floating a, KnownNat d) => Vector d a -> a
norm = sqrt . VS.foldr ((+) . (^2)) 0


dot :: (Unbox a, Floating a, KnownNat d) => Vector d a -> Vector d a -> a
dot = (VS.foldr (+) 0 .) . VS.zipWith (*)


v0 :: R2
v0 = fromTuple (0, 0)

e1 :: R2
e1 = fromTuple (1, 0)

e2 :: R2
e2 = fromTuple (0, 1)
