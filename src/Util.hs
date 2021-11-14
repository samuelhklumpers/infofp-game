module Util where

import Graphics.Gloss.Data.Vector

import Control.Monad.State

import System.Random


uniformF :: Float -> Float -> State StdGen Float
uniformF l h = state $ uniformR (l, h)

uniformbool :: State StdGen Bool
uniformbool = state uniform

v0 :: Vector
v0 = (0, 0)

e1 :: Vector
e1 = (1, 0)

e2 :: Vector
e2 = (0, 1)
