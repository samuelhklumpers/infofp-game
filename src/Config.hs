module Config where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector


import Control.Monad.State

import System.Random


windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 800

--The position of where we draw the scores is based on these numbers
--So if you change these (or find a nice formula)
--also change them in the Drawing module

windowFrame :: (Float, Float)
windowFrame = (fromIntegral windowWidth / 2, fromIntegral windowHeight / 2)

background :: Color
background = black

fps :: Int
fps = 30

playerAccel :: Float
playerAccel = 8

highscoreSize :: Int
highscoreSize = 8

bulletspeed :: Float
bulletspeed  = 300

chaseSpeed :: Float
chaseSpeed = 100

startPosMult :: Float
startPosMult = 1.0

animationspeed :: Float
animationspeed = 100

spawnTick :: Float
spawnTick = 0.1 -- seconds

secondsPerEnemy :: Float
secondsPerEnemy = 3.0

secondsPerAsteroid :: Float
secondsPerAsteroid = 2.0

secondsPerChaser :: Float
secondsPerChaser = 3.0

spawnVXMin :: Float
spawnVXMin = -20

spawnVXMax :: Float
spawnVXMax = 20

spawnVYMin :: Float
spawnVYMin = -200

spawnVYMax :: Float
spawnVYMax = -40


v0 :: Vector
v0 = (0, 0)

e1 :: Vector
e1 = (1, 0)

e2 :: Vector
e2 = (0, 1)
