module GLAdapter where

import Graphics.UI.GLUT

-- Copied from reactive-glut
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  -- putStrLn "resizeScene"
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 75 (w2/h2) 0.001 1000
  matrixMode $= Modelview 0
  flush
 where
   w2 = half width
   h2 = half height
   half z = realToFrac z / 2


