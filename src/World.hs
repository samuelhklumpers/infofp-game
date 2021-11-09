{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module World where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Data.Vector.Unboxed.Sized (fromTuple, toList)

import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)
import Data.Functor.Identity
import Control.Lens
import Control.Monad.State
import Data.Map (empty)

import Being
import Controls
import Util
import Statistics


type Frame = (Float, Float)

data World = World {_frame :: Frame, _beings :: Beings, _keyMap :: KeyMap, _stats:: Stats' Identity, highscores :: [Stats' Identity]} deriving Show
makeLenses ''World


-- drawing
draw :: World -> Picture
draw w = Pictures $ map drawBeing b
    where b = Being.toList $ _beings w

drawBeing :: Being -> Picture
drawBeing Being {_phys = phys, _race = race}
    = color c $ translate x y $ circleSolid r
    where
        Phys {_pos = p, _radius = r} = phys
        (x, y) = p
        c = colorBeing race

colorBeing :: Race -> Color
colorBeing race = case race of
    Player _    -> blue
    Enemy _     -> red
    Asteroid    -> greyN 0.5
    Bullet      -> yellow

-- handlers
handler :: Event -> World -> World
handler e w = case e of
    e'@EventKey {} -> keyMap %~ flip handleKeyState e' $ w
    _              -> w

-- step
step :: Float -> World -> World
step dt =
    fireStep dt .
    damageStep .
    physicsStep dt .
    userStep dt .
    scoreStep dt -- .
--    spawnStep dt 

fireTimeout :: Float 
fireTimeout = 0.3

fireStep :: Float -> World -> World
fireStep dt = execState $ do
    r <- use $ beings . player . race

    case r of 
        Player t -> do
            let t' = min (t + dt) fireTimeout

            fire <- uses keyMap getFire
            if fire && t' >= fireTimeout then do
                beings . player . race .= Player 0
                ph <- use $ beings . player . phys
                let x = ph ^. pos
                let v = ph ^. vel
                let rad = ph ^. radius
                beings <>= terminal (makeBeing Bullet (x Vec.+ (2 * rad) `mulSV` e2) (v Vec.+ 200 `mulSV` e2))
            else
                beings . player . race .= Player t'
        _ -> return ()



-- replace with actual screen bounds if necessary
isInBounds :: Frame -> Vector -> Bool
isInBounds f v = let (x1, y1) = v in let (x2, y2) = f in
    0 <= x1 && x1 <= x2 && 0 <= y1 && y1 <= y2

-- mark everybody that gets hit, e.g. _player hit by bullet -> set damage, asteroid hit by bullet -> exploding, _player hit by asteroid -> death animation 
damageStep :: World -> World
damageStep = execState $ do
    bs <- uses beings Being.toList
    fr <- use frame

    let bs' = filter (isInBounds fr . (^. phys . pos)) bs -- maybe prevent deleting the player
    let bs'' = filter ((>0) . (^. health)) bs'

    beings .= Being.fromList bs''


physicsStep :: Float -> World -> World
physicsStep dt =
    collisionStep .
    freeFallStep dt -- .
    -- otherStep .
    -- etc

scoreStep :: Float -> World -> World
scoreStep dt = stats . survived +~ Identity dt -- I hate Identity


playerAccel :: Float
playerAccel = 8

-- put some upper limit on vel?

userStep :: Float -> World -> World
userStep dt w = beings . player . phys . vel %~ (Vec.+ playerAccel `mulSV` a) $ w
    where a = getAccel (w ^. keyMap)


-- do physics movement, ignoring gravity, collisions, or other sources of acceleration
freeFallStep :: Float -> World -> World
freeFallStep dt w@World {_beings = b} = w {_beings = fmap (freeFall dt) b}

-- do physics collisions, ignoring death on contact and other effects
collisionStep :: World -> World
collisionStep = beings %~ collisions


-- tests
testPlayer :: Being
testPlayer = makeBeing (Player 0) (400 `mulSV` e1 Vec.+ 400 `mulSV` e2) v0

testAsteroid :: Being
testAsteroid = makeBeing Asteroid (400 `mulSV` e1 Vec.+ 100 `mulSV` e2) v0

{-
testEnemy :: Being
testEnemy = Being (Phys (100 * e2) v0 1 20) (Enemy 0)

testBullet :: Being
testBullet = Being (Phys (100 * (e1 + e2)) v0 1 10) Bullet
-}
 
testWorld :: Frame -> World
testWorld frame = World frame (Pointed testPlayer [testAsteroid]) emptyKM (Stats' 0.0) []
