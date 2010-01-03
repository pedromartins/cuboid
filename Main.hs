module Main where

import Graphics.UI.GLUT

data Point3D = P3D { x :: Integer, y :: Integer, z :: Integer }

data Level = Level { size :: Integer, obstacles :: [Point3D] }


-- | main
main :: IO ()
main = do
    getArgsAndInitialize
    createWindow "Anaglyph Cube"
    matrixMode $= Projection
    loadIdentity
    perspective 45.0 1 1 1000
    matrixMode $= Modelview 0
    displayCallback $= do 
        clear [ ColorBuffer ]
        translate $ Vector3 (0 :: Float) 0 (-5)
        renderObject Wireframe (Cube 1)
        flush
    mainLoop
