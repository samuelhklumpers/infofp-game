{-
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
-}


module Util where

import Graphics.Gloss.Data.Vector

{-
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
unit :: (Unbox a, Floating a, KnownNat d) => Vector d a -> Vector d a
unit v = (1/norm v) *| v
dot :: (Unbox a, Floating a, KnownNat d) => Vector d a -> Vector d a -> a
dot = (VS.foldr (+) 0 .) . VS.zipWith (*)
-}


v0 :: Vector
v0 = (0, 0)

e1 :: Vector
e1 = (1, 0)

e2 :: Vector
e2 = (0, 1)
