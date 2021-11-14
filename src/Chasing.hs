module Chasing where

import Being
import World
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State
import Config
{-
 - This module handles the chaser
 - they change their direction towards the player
 -}

chaseTarget :: Vector -> Being -> Being
chaseTarget target = execState $ do
                        r <- use race
                        currentpos <- use (phys . pos)
                        when (r == Chaser) $
                                let direction = normalizeV (target Vec.- currentpos) in
                                (phys . vel) .= (chaseSpeed `mulSV` direction )

chasing :: World -> Beings -> Beings
chasing w = let playerpos = (w ^. beings . player . phys. pos) in
                fmap (chaseTarget playerpos)

chaseStep :: World -> World
chaseStep = execState $ do
                w <- get
                bs <- use beings
                beings .= chasing w bs
