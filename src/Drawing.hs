module Drawing where

import WorldInit
import Being
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import Animations




draw :: World -> Picture
draw w = Pictures $ (map drawBeing b)  ++ (map drawTimedAnimation (_timedAnimations w))
    where b = Being.toList $ _beings w
          --animes = _animes w

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


