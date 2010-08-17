module Config where

import Types

-- TODO: List can't be empty!
testLevel = Level (P3D 0 0 1) (P3D 4 4 5) [P3D 0 0 0, P3D 0 5 1, P3D 5 4 1]
testLevel2 = Level (P3D 0 0 1) (P3D 0 4 1) [P3D 5 5 5]
testLevel3 = Level (P3D 0 1 0) (P3D 4 2 1) [P3D 0 1 1, P3D 1 2 0, P3D 1 3 3,
    P3D 1 1 4, P3D 2 3 0, P3D 3 1 0, P3D 3 4 0, P3D 3 0 2, P3D 3 3 3, P3D 3 1 4,
    P3D 4 2 0] 

levels = concat (repeat [testLevel, testLevel2, testLevel3])

