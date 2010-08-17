{-# LANGUAGE Arrows #-}
module Types where

import FRP.Yampa
import FRP.Yampa.Vector3

import Graphics.UI.GLUT hiding (Level,Vector3(..),normalize)
import qualified Graphics.UI.GLUT as G(Vector3(..))

type R = GLdouble

data Point3D = P3D { x :: Integer, y :: Integer, z :: Integer } deriving (Show)

data Level = Level { startingPoint :: Point3D, 
                     endPoint      :: Point3D,
                     obstacles     :: [Point3D] }

data Input = Keyboard { key       :: Key,
                        keyState  :: KeyState,
                        modifiers :: Modifiers }

data GameState = Game { level     :: Level,
                        rotX      :: R, 
                        playerPos :: Vector3 R }

data ParsedInput = 
    ParsedInput { wCount :: Double, aCount :: Double, 
                  sCount :: Double, dCount :: Double,
                  upEvs  :: Event Input, downEvs :: Event Input, 
                  rightEvs :: Event Input, leftEvs :: Event Input }

