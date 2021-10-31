module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game

import World


-- put all gloss and aeson stuff here

w :: Int
w = 200
h :: Int
h = 200
s = fromIntegral $ min w h

window = InWindow "Nice Window" (w, h) (10, 10)
background = white
fps = 30
world = testWorld


scaler = scale s s


main :: IO ()
main = playIO window background fps world (return . scaler . draw) ((return .) . handler) ((return .) . step)

-- ((return .) . handler)

debugHandler :: Event -> World -> IO World
debugHandler e w = do
    print e
    return w

