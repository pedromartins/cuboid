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
    oldTime <- newIORef (0 :: Int)
    rh <- reactInit (initGL >> return NoEvent) (\_ _ b -> b >> return False) 
                    (parseInput >>> calculateState >>> game)
    displayCallback $= return ()
    keyboardMouseCallback $= Just 
        (\k ks m _ -> writeIORef newInput (Event $ Keyboard k ks m))
    idleCallback $= Just (idle newInput oldTime rh)
    oldTime' <- get elapsedTime
    writeIORef oldTime oldTime' 
    mainLoop

-- | Reactimation iteration, supplying the input
idle :: IORef (Event Input) -> IORef Int -> 
        ReactHandle (Event Input) (IO ()) -> IO ()
idle newInput oldTime rh = do
    newInput' <- readIORef newInput
    newTime'  <- get elapsedTime
    oldTime'  <- get oldTime
    let dt = let dt' = (fromIntegral $ newTime' - oldTime')/50
             in if dt' < 1 then dt' else 1 
    react rh (dt, Just newInput')
    writeIORef oldTime newTime'
    return ()
    
