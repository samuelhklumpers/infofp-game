module Config where

import Graphics.Gloss.Data.Color


w :: Int
h :: Int
w = 800
h = 800
--The position of where we draw the scores is based on these numbers
--So if you change these (or find a nice formula)
--also change them in the Drawing module

windowFrame :: (Float, Float)
windowFrame = (fromIntegral w / 2, fromIntegral h / 2)

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

startPosMult :: Float
startPosMult = 1.0
