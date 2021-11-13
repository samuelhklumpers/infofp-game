module Shooting where

import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Interact
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State

import Being
import WorldInit
import Data.Maybe
import Debug.Trace (traceShow)

shoottimeout :: TimeSinceLastShot
shoottimeout = 0.3

bulletspeed :: Float
bulletspeed  = 300

startPosMult :: Float
startPosMult = 1.5


shootBullet :: Being -> Vector -> Maybe Being
--In principe kan alles schieten, leuk als je enemies maakt die turrets kunnen plaatsen op asteroids zodat asteroids op de player schieten. 
shootBullet shooter targetpos  | time < shoottimeout = Nothing
                               | otherwise    = Just (makeBeing Bullet startpos velocity) where
                                   time       = shooter ^. timeSinceLastShot
                                   shooterpos = shooter ^. phys. pos
                                   direction  = normalizeV (targetpos Vec.- shooterpos)
                                   velocity   = bulletspeed `mulSV` direction Vec.+ shooter ^. phys . vel
                                   startpos   = shooterpos  Vec.+ ((startPosMult * (shooter ^. phys.radius)) `mulSV` direction)

playerShot :: World -> Maybe Being
playerShot w = do
    target <- w ^. userIn . firing
    shootBullet (w ^. beings . player) target

fireStep :: Float -> World -> World
fireStep dt = execState $ do
    w <- get
    case playerShot w of
        Nothing     -> return ()
        Just bullet -> do
            beings . player . timeSinceLastShot .= 0
            spawnBeing bullet


unpackAI :: Being -> Maybe (Being, Race, AimAI, MoveAI)
unpackAI b@Being {_race = e@(Enemy _ shootingAI movingAI)} = Just (b, e, shootingAI, movingAI)
unpackAI _ = Nothing

shootIfAiAlgebra :: Beings -> Being -> ([Being], State World ()) -> ([Being], State World ())
shootIfAiAlgebra others self (prev, fx) = let (self', fx') = shootIfAi others self in (self':prev, fx >> fx')

shootIfAi :: Beings -> Being -> (Being, State World ())
shootIfAi others self = fromMaybe (self, return ()) $ do
    (b, r, aimAI, _) <- unpackAI self
    target <- aimAI self others
    bullet <- shootBullet self target

    return (
        self & timeSinceLastShot .~ 0,
        spawnBeing bullet)
    

aiShootStep :: Beings -> [Being] -> ([Being], State World ())
aiShootStep bs = foldr (shootIfAiAlgebra bs) ([], return ())

aiStep :: Float -> World -> World
aiStep dt = execState $ do
    bs1 <- use beings
    let bs = toList bs1

    let (bs', effects) = aiShootStep bs1 bs

    beings .= fromList bs'
    effects -- this one goes after :)
