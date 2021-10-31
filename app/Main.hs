module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import World


-- put all gloss and aeson stuff here

window = (InWindow "Nice Window" (200, 200) (10, 10))
background = white
fps = 30
world = testWorld



main = play window background fps world draw handler step