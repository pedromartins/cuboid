{-# LANGUAGE Arrows #-}
module Main where

import FRP.Yampa
import FRP.Yampa.Utilities
import Graphics.UI.GLUT hiding (Level)

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

-- size is unncessary!!
data Level = Level { obstacles :: [Point3D] }

size :: Level -> Integer
size = (+1) . maximum . map (\(P3D x y z) -> maximum [x,y,z]) . obstacles

data GameState = Game { rotX :: Double, rotY :: Double }

type R = Double

testLevel = Level [P3D 0 0 0, P3D 5 5 5]

xAxis = Vector3 1 0 0 :: Vector3 R 
yAxis = Vector3 0 1 0 :: Vector3 R
zAxis = Vector3 0 0 1 :: Vector3 R 

initGL :: IO (Event Input)
initGL = do
    getArgsAndInitialize
    createWindow "AnaCube!"
    clearColor $= Color4 0 0 0 0
    reshapeCallback $= Just resizeScene
    return NoEvent

renderGame :: Level -> GameState -> IO ()
renderGame l (Game rotX rotY) = do
    loadIdentity
    translate $ Vector3 (0 :: R) 0 (-2*(fromInteger $ size l))
    rotate (rotX * 10) xAxis
    rotate (rotY * 10) yAxis
    renderObject Wireframe (Cube $ fromInteger $ size l)
    mapM_ renderObstacle $ obstacles l
    flush
    where size2 :: R
          size2 = (fromInteger $ size l)/2
          renderObstacle p3D = preservingMatrix $ do
            translate $ Vector3 (0.5 - size2 + (fromInteger $ x p3D)) 
                                (0.5 - size2 + (fromInteger $ y p3D)) 
                                (0.5 - size2 + (fromInteger $ z p3D))
            renderObject Solid (Cube 1)

keyDowns :: SF (Event Input) (Event Input)
keyDowns = arr $ filterE ((==Down) . keyState)

countHold :: SF (Event a) Integer
countHold = count >>> hold 0

game :: SF GameState (IO ())
game = arr $ (\gs -> do
        clear [ ColorBuffer ]
        renderGame testLevel gs
        flush)

-- | Input
parseInput :: SF (Event Input) GameState
parseInput = proc i -> do
    down <- keyDowns     -< i
    ws   <- countKey 'w' -< down
    as   <- countKey 'a' -< down
    ss   <- countKey 's' -< down
    ds   <- countKey 'd' -< down
    returnA -< Game (fromInteger $ (ws - ss)) (fromInteger $ (ds - as))

    where countKey c = filterE ((==(Char c)) . key) ^>> countHold

-- | Main, initializes Yampa and sets up reactimation loop
main :: IO ()
main = do
    newInput <- newIORef NoEvent
    rh <- reactInit initGL (\_ _ b -> b >> return False) 
                    (parseInput >>> game)
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
    
