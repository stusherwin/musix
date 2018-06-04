module Keyboard (
  Keyboard (),
  makeKeyboard,
  keyDown,
  keyUp,
  firstKey,
  lastKey,
  notes
) where

import Graphics
import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))
import Chords

data PlayState = Playing
               | NotPlaying deriving (Show, Eq)

data Keyboard = Keyboard (M.Map Int PlayState) deriving (Show, Eq)

makeKeyboard :: Int -> Int -> Keyboard
makeKeyboard start end = Keyboard $ M.fromList $ take (end - start + 1) . (drop start) . (zip [0..]) $ repeat NotPlaying

keyDown :: Int -> Keyboard -> Keyboard
keyDown key (Keyboard keyMap) = Keyboard $ M.update (\_ -> Just Playing) key keyMap

keyUp :: Int -> Keyboard -> Keyboard
keyUp key (Keyboard keyMap) = Keyboard $ M.update (\_ -> Just NotPlaying) key keyMap

firstKey :: Keyboard -> Int
firstKey (Keyboard keyMap) = fst $ M.findMin keyMap

lastKey :: Keyboard -> Int
lastKey (Keyboard keyMap) = fst $ M.findMax keyMap

notes :: Keyboard -> [Note]
notes (Keyboard keyMap) = map (toNote . fst) $ filter (\(k, p) -> playing p) (M.toList keyMap)

playing Playing = True
playing _ = False

data WhiteKeyType = FullKey
                  | LeftKey
                  | MiddleKey
                  | RightKey deriving (Show)

data Key = White WhiteKeyType
         | Black deriving (Show)

keyboardLayout :: M.Map Int Keyboard.Key
keyboardLayout = M.fromList $ zip [0..] [ White LeftKey
                                        , Black
                                        , White MiddleKey
                                        , Black
                                        , White RightKey
                                        , White LeftKey
                                        , Black
                                        , White MiddleKey
                                        , Black
                                        , White MiddleKey
                                        , Black
                                        , White RightKey ]

isWhite (White _) = True
isWhite _ = False

isBlack Black = True
isBlack _ = False

blackSize = V2 16 76
whiteSize = V2 26 116
gap = 2
blackCol = makeGColor 0.5 0.5 0.5
whiteCol = makeGColor 1 1 1
redCol = makeGColor 1 0 0
keyboardScale = 3 --0.01

instance Drawable Keyboard where
  draw keyboard@(Keyboard keyMap) origin = do
    mapM_ drawKey $ M.keys keyMap
    where
      keyTypes = M.mapWithKey (\k _ -> keyboardLayout ! (k `mod` 12)) keyMap
      noOfWhites key = fromIntegral $ M.size $ (M.filter isWhite) $ fst $ M.split key keyTypes
      drawKey :: Int -> IO ()
      drawKey key = do
        case (keyType, (key == firstKey keyboard), (key == lastKey keyboard)) of
          (White LeftKey, _, False) -> whiteLeft
          (White LeftKey, _, True) -> whiteFull
          (White MiddleKey, False, False) -> whiteMiddle
          (White MiddleKey, True, _) -> whiteLeft
          (White MiddleKey, _, True) -> whiteRight
          (White RightKey, False, _) -> whiteRight
          (White RightKey, True, _) -> whiteFull
          (Black, _, _) -> black
        where
          playState = keyMap ! key
          keyType = keyTypes ! key
          keyColor = case (playState, keyType) of
                       (Playing, _) -> redCol
                       (_, White _) -> whiteCol
                       (_, Black) -> blackCol
          whiteLeft = do
            whiteBase
            drawRect whiteOffset (V2 0 0) (V2 (x whiteSize - x blackInset) (y blackSize))
          whiteMiddle = do
            whiteBase
            drawRect whiteOffset (V2 (x blackInset) 0) (V2 (x whiteSize - x blackInset) (y blackSize))
          whiteRight = do
            whiteBase
            drawRect whiteOffset (V2 (x blackInset) 0) (V2 (x whiteSize) (y blackSize))
          whiteFull = do
            drawRect whiteOffset (V2 0 0) whiteSize
          black = do
            drawRect blackOffset (V2 0 0) blackSize
          whiteBase = drawRect (whiteOffset |+| V2 0 (y blackSize)) (V2 0 0) (V2 (x whiteSize) (y blackInset))
          blackInset = V2 ((x blackSize + gap) / 2) (y whiteSize - y blackSize)
          whiteOffset = V2 (noOfWhites key * (x whiteSize + gap)) 0
          blackOffset = whiteOffset |+| V2 (-(x blackInset)) (-gap)--(y blackInset + gap)
          drawRect offset p1 p2 =
            draw (GRect keyColor (p1 |*| keyboardScale) (p2 |*| keyboardScale)) (origin |+| (offset |*| keyboardScale))