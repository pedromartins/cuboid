module Main where

import FRP.Yampa
import Graphics.UI.GLUT

import Data.IORef
import Control.Arrow
import Data.Monoid

import GLAdapter

-- | Event Definition:

data Input = Keyboard { key       :: Key,
                        keyState  :: KeyState,
                        modifiers :: Modifiers }
-- | Rendering Code:

data Point3D = P3D { x :: Integer, y :: Integer, z :: Integer }

data Level = Level { size :: Integer, obstacles :: [Point3D] }

type R = Double

xAxis = Vector3 1 0 0 :: Vector3 R 
yAxis = Vector3 0 1 0 :: Vector3 R
zAxis = Vector3 0 0 1 :: Vector3 R 

initGL :: IO (Event Input)
initGL = do
    getArgsAndInitialize
    createWindow "Hello World"
    clearColor $= Color4 0 0 0 0
    reshapeCallback $= Just resizeScene
    return NoEvent

cube :: Double -> IO ()
cube rot = do
    loadIdentity
    translate $ Vector3 (0 :: R) 0 (-2)
    rotate (rot * 100) yAxis
    renderObject Wireframe (Cube 1)
    flush

game :: SF (Event Input) (IO ())
game = time >>^ (\rot -> do 
    clear [ ColorBuffer ]
    cube rot
    flush)

-- | Main, initializes Yampa and sets up reactimation loop
main :: IO ()
main = do
    newInput <- newIORef NoEvent
    rh <- reactInit initGL (\_ _ b -> b >> return False) game
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
    
