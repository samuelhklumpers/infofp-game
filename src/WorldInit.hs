{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module WorldInit where

import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import GHC.TypeNats (KnownNat)
import Data.Functor.Identity
import Control.Lens
import Control.Monad.State
import Data.Map (empty, member)
import System.Random
import System.Random.Stateful


import Being
import Util
import Statistics

type Frame = (Float, Float)
data SpawnData = SpawnData {_timeSinceLast :: Float, _asteroidRate :: Float, _enemyRate :: Float} deriving Show
makeLenses ''SpawnData

data MotionControl = MotionControl {
    _motionU :: Bool,
    _motionR :: Bool,
    _motionD :: Bool,
    _motionL :: Bool
}
makeLenses ''MotionControl

data UserInput = UserInput {
    _pausing :: Bool,
    _firing  :: Bool,
    _moving  :: MotionControl
}
makeLenses ''UserInput

blankMotion :: MotionControl
blankMotion = MotionControl False False False False

blankInput :: UserInput
blankInput = UserInput False False blankMotion

data World = World {
    _frame :: Frame,
    _beings :: Beings,
    _userIn :: UserInput,
    _stats :: Stats' Identity,
    _spawns :: SpawnData,
    _randomizer :: StdGen,
    highscores :: [Stats' Identity]
}
makeLenses ''World

instance Show World where
    show w = ":(" -- put stuff here StateGenM doesn't print nice

{-

-- handlers
handler :: Event -> World -> World
handler = handleInput


handleInput :: Event -> World -> World
handleInput e = execState $ do
    case e of
        e'@(EventKey key s _ _)   -> do
            if member key dirMap then
                keyMap %= flip handleKeyState e'
            else
                when (key == Char 'p' && s == Down) $ paused %= not
        _                       -> return ()


-- step
step :: Float -> World -> World
step dt = execState $ do
    p <- use paused

    unless p $ do
        modify $ fireStep dt
        modify damageStep
        modify $ physicsStep dt
        modify $ userStep dt
        modify $ scoreStep dt
        modify $ spawnStep dt

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
                spawnBeing (makeBeing Bullet (x Vec.+ (2 * rad) `mulSV` e2) (v Vec.+ 200 `mulSV` e2))
            else
                beings . player . race .= Player t'
        _ -> return ()


uniformF :: Float -> Float -> State StdGen Float
uniformF l h = state $ uniformR (l, h)


spawnStep :: Float -> World -> World
spawnStep dt = execState $ do
    spawns . timeSinceLast += dt
    t <- use (spawns . timeSinceLast)
    rate <- use (spawns . asteroidRate)
    (w, h) <- use frame
    -- put enemy here too

    let n = round (t / spawnTick)
    spawns . timeSinceLast -= fromIntegral n * spawnTick

    forM_ [1..n] $ \_ -> do -- lol
        ret <- zoom randomizer $ do
            roll <- uniformF 0.0 1.0

            if roll < rate then do
                x <- uniformF 0.0 w
                vx <- uniformF (-20) 20
                vy <- uniformF (-40) (-200)
                let y = 600.0
                return $ Just (x, y, vx, vy)
            else return Nothing

        case ret of
            Just (x, y, vx, vy) -> spawnBeing (makeBeing Asteroid (x, y) (vx, vy))
            Nothing -> return ()


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

testWorld :: Frame -> IO World
testWorld frame = do
    rng <- newStdGen

    return $ World frame (Pointed testPlayer [testAsteroid]) emptyKM (Stats' 0.0) (SpawnData 0 (toRate 0.5) 0) rng False []

-}