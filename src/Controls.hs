{-# LANGUAGE TemplateHaskell #-}


module Controls where
{-In the game, we keep track of the user input and handle the user input in this module
 -}


import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State
import Data.Maybe

import Config
import World


isDown :: Event -> Bool
isDown (EventKey _ s _ _) = s == Down
isDown _ = False

handleInput :: Event -> World -> World
handleInput e w = fromMaybe w $ flip execStateT w $ do
    w <- get
    let keyDown = isDown e

    when (w ^. gameState == GameOver) (lift Nothing)

    zoom userIn $ do
        case e of
            EventMotion newpos -> firing %= (>> return newpos)
            EventKey (MouseButton LeftButton) Down  _ mousepos -> firing .= Just mousepos
            EventKey (MouseButton LeftButton) Up    _ _        -> firing .= Nothing
            EventKey (Char c) _ _ _ -> case c of
                'w' -> moving . motionU .= keyDown
                'a' -> moving . motionL .= keyDown
                's' -> moving . motionD .= keyDown
                'd' -> moving . motionR .= keyDown
                _   -> return ()
            _ -> return ()

    case e of
        EventKey (Char c) Down _ _ -> case c of
            'q' -> gameState .= PlayerDied
            'p' -> do
                gs <- use gameState
                case gs of
                    Playing -> gameState .= Pausing
                    Pausing -> gameState .= Playing
                    _       -> return ()
            _ -> return ()
        _ -> return ()


toggleAccel :: Vector -> Bool -> Vector -> Vector
toggleAccel v b w
    | b         = v Vec.+ w
    | otherwise = w

getAccel :: MotionControl -> Vector
getAccel (MotionControl u r d l) =
    toggleAccel e2 u $
    toggleAccel e1 r $
    toggleAccel (Vec.negate e2) d $
    toggleAccel (Vec.negate e1) l v0