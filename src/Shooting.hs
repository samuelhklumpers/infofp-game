module Shooting where
{-
 - This module handles how we proces a shot, and has the shooting AI of enemies which try to shoot the player
 - When a Being tries to shoot a certain piece of ammo, we first check whether that being is allowed to shoot
 - then create a Bullet of specified ammo in the direction the shooter is aiming. 
 -}
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Interact
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State
import Data.Maybe

import Being
import World
import Config


shootBullet :: Being -> Race -> Vector -> Maybe Being
--In principe kan alles schieten, leuk als je enemies maakt die turrets kunnen plaatsen op asteroids zodat asteroids op de player schieten. 
shootBullet shooter ammo targetpos  | canShoot shooter = Just (makeBeing ammo startpos velocity) 
                                    | otherwise    = Nothing where 
                                        time       = shooter ^. timeSinceLastShot
                                        shooterpos = shooter ^. phys. pos
                                        direction  = normalizeV (targetpos Vec.- shooterpos)
                                        velocity   = bulletspeed `mulSV` direction Vec.+ shooter ^. phys . vel
                                        startpos   = shooterpos  Vec.+ ((startPosMult * (shooter ^. phys.radius + radiusBeing ammo)) `mulSV` direction)

playerShot :: World -> Maybe Being
playerShot w = do
    target <- w ^. userIn . firing
    shootBullet (w ^. beings . player) Bullet target

fireStep :: Float -> World -> World
fireStep dt = execState $ do
    w <- get
    case playerShot w of
        Nothing     -> return ()
        Just bullet -> do
            beings . player . timeSinceLastShot .= 0
            spawnBeing bullet


unpackAI :: Being -> Maybe (Being, Race, AimAI, MoveAI)
unpackAI b@Being {_race = e@(Enemy shootingAI movingAI)} = Just (b, e, shootingAI, movingAI)
unpackAI _ = Nothing

shootIfAiAlgebra :: Beings -> Being -> ([Being], State World ()) -> ([Being], State World ())
shootIfAiAlgebra others self (prev, fx) = let (self', fx') = shootIfAi others self in (self':prev, fx >> fx')

shootIfAi :: Beings -> Being -> (Being, State World ())
shootIfAi others self = fromMaybe (self, return ()) $ do
    (b, r, aimAI, _) <- unpackAI self
    target <- aimAI self others
    bullet <- shootBullet self Bullet target

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
