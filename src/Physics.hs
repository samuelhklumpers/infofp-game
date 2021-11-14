module Physics where


import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec

import Control.Lens
import Data.Array.MArray
import Control.Monad
import Control.Monad.ST
import Data.Array.ST

import Pointed
import Being

-- move assuming no gravity or collision
freeFall :: Float -> Being -> Being
freeFall dt = phys %~ freeFall' dt

freeFall' :: Float -> Phys -> Phys
freeFall' dt p = pos %~ (Vec.+ mulSV dt (p ^.vel)) $ p

-- find normal vector in collision of two beings
collide :: Being -> Being -> Maybe Vector
collide a b
    | d1 <= d2  = Just v
    | otherwise = Nothing where
        v = (a ^. phys . pos) Vec.- (b ^. phys . pos)
        d1 = magV v
        d2 = (a ^. phys . radius) + (b ^. phys . radius)

-- collision helper
collisions :: Beings -> Beings
collisions = fromList . doCollisions . toList

-- perform elastic collision of two beings
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

-- perform all collisions between all beings, in mutable state
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