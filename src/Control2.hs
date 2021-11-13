{-# LANGUAGE TemplateHaskell #-}


module Control2 where

import WorldInit
import Shooting
import Being
import Graphics.Gloss.Interface.Pure.Game
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Control.Lens
import Control.Monad.State
import Data.Maybe


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
