{-# LANGUAGE Arrows #-}
module Input where

import FRP.Yampa
import FRP.Yampa.Utilities

import Graphics.UI.GLUT

-- | Event Definition:

data Input = Keyboard { key       :: Key,
                        keyState  :: KeyState,
                        modifiers :: Modifiers }

keyDowns :: SF (Event Input) (Event Input)
keyDowns = arr $ filterE ((==Down) . keyState)

countHold :: SF (Event a) Integer
countHold = count >>> hold 0

data ParsedInput = 
    ParsedInput { ws :: Integer, as :: Integer, ss :: Integer, ds :: Integer,
                  upEvs    :: Event Input, downEvs :: Event Input, 
                  rightEvs :: Event Input, leftEvs :: Event Input }
                        
-- | Input
parseInput :: SF (Event Input) ParsedInput
parseInput = proc i -> do
    down     <- keyDowns                        -< i
    ws       <- countKey 'w'                    -< down
    as       <- countKey 'a'                    -< down
    ss       <- countKey 's'                    -< down
    ds       <- countKey 'd'                    -< down
    upEvs    <- filterKey (SpecialKey KeyUp)    -< down
    downEvs  <- filterKey (SpecialKey KeyDown)  -< down
    rightEvs <- filterKey (SpecialKey KeyRight) -< down
    leftEvs  <- filterKey (SpecialKey KeyLeft)  -< down
    returnA -< ParsedInput ws as ss ds upEvs downEvs rightEvs leftEvs
    where countKey c  = filterE ((==(Char c)) . key) ^>> countHold
          filterKey k = arr $ filterE ((==k) . key)


