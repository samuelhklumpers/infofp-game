module Init where


import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture

import Graphics.Gloss.Data.Vector
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Data.Color (greyN)
import Data.Maybe
import Data.Functor.Identity
import Data.List
import Control.Lens
import Control.Monad.State
import Control.Monad
import Data.Map (empty, member, Map)
import System.Random.Stateful

import World
import Being
import Controls
import Shooting
import Statistics
import Reaping
import Spawning
import Config
import Drawing
import Animations
import Rules


window :: Display
window = InWindow "Awesome Asteroids Game" (windowWidth, windowHeight) (10, 10)

maximum' :: [Identity Int] -> Identity Int
maximum' [] = 0
maximum' xs = maximum xs

makeStats :: [Stats] -> Stats
makeStats xs = blankStats & attempt .~ maximum' (map _attempt xs) + 1


initPlayer :: Being
initPlayer = makeBeing Player v0 v0


initWorld :: IO World
initWorld = do
    scores <- fromMaybe [] <$> (jload "scores.json" :: IO (Maybe [Stats]))

    let stats' = makeStats scores

    rng <- newStdGen

    return $ World
        windowFrame
        (Pointed initPlayer [])
        blankInput
        stats'
        baseSpawnRates
        rng Playing
        scores
        []
