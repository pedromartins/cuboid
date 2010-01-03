module Main where

import FRP.Reactive
import FRP.Reactive.GLUT.Adapter as RG
import Graphics.UI.GLUT

import Control.Applicative
import Data.Monoid

data Point3D = P3D { x :: Integer, y :: Integer, z :: Integer }

data Level = Level { size :: Integer, obstacles :: [Point3D] }

type R = Double

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 75 (w2/h2) 0.001 100
  matrixMode $= Modelview 0
  flush
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2

xAxis = Vector3 1 0 0 :: Vector3 R 
yAxis = Vector3 0 1 0 :: Vector3 R
zAxis = Vector3 0 0 1 :: Vector3 R 

initGL :: IO ()
initGL = do
    getArgsAndInitialize
    createWindow "Hello World"
    clearColor $= Color4 0 0 0 0
    reshapeCallback $= Just resizeScene

sphere rot = do
    loadIdentity
    translate $ Vector3 (0 :: R) 0 (-2)
    rotate (rot * 100) yAxis
    renderObject Wireframe (Cube 1)
    flush


justE' = (>>= maybe mempty return)

rSphere :: UI -> Behavior Action
rSphere ui = sphere <$> (countB $ justE' (fmap Just $ keyAction ui))

-- | main
main :: IO ()
main = do 
    initGL
    adapt rSphere
