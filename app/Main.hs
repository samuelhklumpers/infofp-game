module Main where


import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import Data.Aeson
import Data.Functor.Identity
import Control.Lens
import Data.Bifunctor
import Data.Maybe ( fromMaybe )
import Data.IORef
import Data.Map as M
import qualified Data.Vector.Unboxed.Sized as VS
import Control.Exception
import System.Exit

--import World
import WorldInit
import WorldExec
import Statistics
import Util
import Drawing


w :: Int
h :: Int
w = 800
h = 800

windowFrame :: (Float, Float)
windowFrame = (fromIntegral w, fromIntegral h)


window :: Display
window = InWindow "Nice Window" (w, h) (10, 10)

background :: Color
background = black

fps :: Int
fps = 30

initWorld :: IO World
initWorld = testWorld windowFrame


screenTransform :: Picture -> Picture
screenTransform = translate (-fromIntegral w / 2) (-fromIntegral h / 2)


stepHandle :: Float -> World -> IO World
stepHandle dt w = do
    case M.lookup (SpecialKey KeyEsc) (w ^. keyMap) of
        Just Down -> do
            let score = w ^. stats
            let scores = highscores w

            print "saving scores.."
            jdump (score:scores) "scores.json" -- it seems that this somehow locks the file even after gloss dies..

            exitSuccess -- TODO enter highscore screen here
        _ -> do
            return $ step dt w

main :: IO ()
main = do
    scores <- fromMaybe [] <$> (jload "scores.json" :: IO (Maybe [Stats' Identity]))

    world <- initWorld

    playIO window background fps (world {highscores = scores}) (return . screenTransform . draw) ((return .) . handler) stepHandle

-- old comments from before giving up on capturing window closing
    -- gloss doesn't give us our Worlds back when it finishes, so I don't see how I'm supposed to get things back in forced exits..
    -- the alternative is to drop everything on ESC or close, and write our cleanup code inside gloss...
    -- so I'll just keep an ioref and pretend nothing weird is happening....
    -- GLUT seems to call exitWith from System.Exit, which makes cleanup pretty messy
    -- make sure we only catch if it's actually a GLUT exit
    -- update: it's not an ExitCode, let's catch everything
    -- update 2: closing the window doesn't even raise an Exception..
    -- ps: i'm well aware that IORef is about as bad as unsafeCoerce#


debugHandler :: Event -> World -> IO World
debugHandler e w = do
    print e
    return w
