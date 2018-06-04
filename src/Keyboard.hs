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
import Data.List (zip4)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))
import Chords

data PlayState = Playing
               | NotPlaying deriving (Show, Eq)

data Keyboard = Keyboard (Maybe Scale) (M.Map Int PlayState) deriving (Show, Eq)

makeKeyboard :: Int -> Int -> Keyboard
makeKeyboard start end = Keyboard (Just $ Scale Ab Major) $ M.fromList $ take (end - start + 1) . (drop start) . (zip [0..]) $ repeat NotPlaying

keyDown :: Int -> Keyboard -> Keyboard
keyDown key (Keyboard scale keyMap) = Keyboard scale $ M.update (\_ -> Just Playing) key keyMap

keyUp :: Int -> Keyboard -> Keyboard
keyUp key (Keyboard scale keyMap) = Keyboard scale $ M.update (\_ -> Just NotPlaying) key keyMap

firstKey :: Keyboard -> Int
firstKey (Keyboard _ keyMap) = fst $ M.findMin keyMap

lastKey :: Keyboard -> Int
lastKey (Keyboard _ keyMap) = fst $ M.findMax keyMap

notes :: Keyboard -> [Note]
notes (Keyboard _ keyMap) = map (toNote . fst) $ filter (\(k, p) -> playing p) (M.toList keyMap)

playing Playing = True
playing _ = False

data WhiteKeyType = L
                  | M
                  | R deriving (Show)

data Key = W WhiteKeyType
         | B deriving (Show)

keyboardLayout :: M.Map Int Keyboard.Key
keyboardLayout = M.fromList $ zip [0..] [ W L
                                        , B
                                        , W M
                                        , B
                                        , W R
                                        , W L
                                        , B
                                        , W M
                                        , B
                                        , W M
                                        , B
                                        , W R
                                        ]

blackKeySize = V2 16 76
whiteKeySize = V2 26 116
gap = 2

black = makeGColor 0.5 0.5 0.5
white = makeGColor 1 1 1
red = makeGColor 1 0 0
blue = makeGColor 0 0 1
  
instance Drawable Keyboard where
  draw keyboard@(Keyboard scale keyMap) origin (V2 w h) = do
    mapM_ drawKey keyData
    where
      keyData = zip4 keys (M.elems keyMap) keyTypes allowed
      keys = M.keys keyMap
      keyTypes = map (\k -> keyboardLayout ! (k `mod` 12)) keys
      allowed = maybe (repeat True) (\s -> map (inScale s . toNote) keys) scale
      noOfWhites key = fromIntegral $ ((key `div` 12) * 7) + ([0,1,1,2,2,3,4,4,5,5,6,6] !! (key `mod` 12))
      drawKey :: (Int, PlayState, Key, Bool) -> IO ()
      drawKey (key, playState, keyType, allowed) = do
        case (keyType, (key == firstKey keyboard), (key == lastKey keyboard)) of
          (W L, _, False) -> drawWhiteKeyLeft
          (W L, _, True) -> drawWhiteKeyFull
          (W M, False, False) -> drawWhiteKeyMiddle
          (W M, True, _) -> drawWhiteKeyLeft
          (W M, _, True) -> drawWhiteKeyRight
          (W R, False, _) -> drawWhiteKeyRight
          (W R, True, _) -> drawWhiteKeyFull
          (B, _, _) -> drawBlackKey
        where
          keyboardWidth = ((fromIntegral $ M.size keyMap) * 7.0 / 12.0) * (x whiteKeySize + gap)
          keyboardHeight = y whiteKeySize
          keyboardScale = V2 (w / keyboardWidth) (h / keyboardHeight)
          keyColor = case (playState, allowed, keyType) of
                       (Playing, False, _) -> red
                       (Playing, True, _) -> blue
                       (_, _, W _) -> white
                       (_, _, B) -> black
          drawWhiteKeyLeft = do
            drawWhiteBase
            drawRect' whiteKeyOffset (V2 0 0) (V2 (x whiteKeySize - x blackKeyInset) (y blackKeySize))
          drawWhiteKeyMiddle = do
            drawWhiteBase
            drawRect' whiteKeyOffset (V2 (x blackKeyInset) 0) (V2 (x whiteKeySize - x blackKeyInset) (y blackKeySize))
          drawWhiteKeyRight = do
            drawWhiteBase
            drawRect' whiteKeyOffset (V2 (x blackKeyInset) 0) (V2 (x whiteKeySize) (y blackKeySize))
          drawWhiteKeyFull = do
            drawRect' whiteKeyOffset (V2 0 0) whiteKeySize
          drawBlackKey = do
            drawRect' blackKeyOffset (V2 0 0) blackKeySize
          drawWhiteBase = drawRect' (whiteKeyOffset |+| V2 0 (y blackKeySize)) (V2 0 0) (V2 (x whiteKeySize) (y blackKeyInset))
          blackKeyInset = V2 ((x blackKeySize + gap) / 2) (y whiteKeySize - y blackKeySize)
          whiteKeyOffset = V2 (noOfWhites key * (x whiteKeySize + gap)) 0
          blackKeyOffset = whiteKeyOffset |+| V2 (-(x blackKeyInset)) (-gap)
          drawRect' offset p1 p2 =
            drawRect keyColor (origin |+| (offset |**| keyboardScale)) (p1 |**| keyboardScale) (p2 |**| keyboardScale)