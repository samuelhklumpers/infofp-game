module Main where


import Graphics.Gloss


import GameState


-- put all gloss and aeson stuff here

main = display (InWindow "Nice Window" (200, 200) (10, 10)) white (Circle 80)