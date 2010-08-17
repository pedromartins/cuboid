module Utils where

import FRP.Yampa.Vector3

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)

import Types

p3DtoV3 ::  (RealFloat a) => Point3D -> Vector3 a
p3DtoV3 (P3D x y z) = vector3 (fromInteger x) (fromInteger y) (fromInteger z)

vectorApply f v = vector3 (f $ vector3X v) (f $ vector3Y v) (f $ vector3Z v)

vector3Rotate' :: (Integral a, RealFloat b) => a -> Vector3 b -> Vector3 b
vector3Rotate' theta v =
  let rotateTheta 0 v = id v                                
      rotateTheta 1 v = vector3 (vector3X v) (vector3Z v)    (-(vector3Y v))
      rotateTheta 2 v = vector3 (vector3X v) (-(vector3Y v)) (-(vector3Z v)) 
      rotateTheta 3 v = vector3 (vector3X v) (-(vector3Z v))   (vector3Y v)
      rotateTheta i _ = rotateTheta (abs $ i `mod` 4) v
  in rotateTheta theta $ v

-- TODO: Memoize
size :: Level -> Integer
size = (+1) . maximum . map (\(P3D x y z) -> maximum [x,y,z]) . obstacles


