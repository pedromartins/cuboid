{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)

import Data.IORef

import Graphics
import Input
import GameLogic

-- | Main, initializes Yampa and sets up reactimation loop
main :: IO ()
main = do
    newInput <- newIORef NoEvent
    rh <- reactInit (initGL >> return NoEvent) (\_ _ b -> b >> return False) 
                    (parseInput >>> calculateState >>> game)
    displayCallback $= return ()
    keyboardMouseCallback $= Just 
        (\k ks m _ -> writeIORef newInput (Event $ Keyboard k ks m))
    idleCallback $= Just (idle newInput rh) 
    mainLoop

-- | Reactimation iteration, supplying the input
idle :: IORef (Event Input) -> ReactHandle (Event Input) (IO ()) -> IO ()
idle newInput rh = do
    newInput' <- readIORef newInput
    react rh (1, Just newInput')
    return ()
    
