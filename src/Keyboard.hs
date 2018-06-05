module Keyboard (
  Keyboard (),
  makeKeyboard,
  keyDown,
  keyUp,
  firstKey,
  lastKey,
  notesPlaying,
  drawKeyboard
) where

import qualified Graphics.UI.GLUT as G
import Data.List (zip4)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))

import Music
import Graphics

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

notesPlaying :: Keyboard -> [Note]
notesPlaying (Keyboard keyMap) = map (toNote . fst) $ filter (\(k, p) -> playing p) (M.toList keyMap)

playing Playing = True
playing _ = False

data WhiteKeyType = L
                  | M
                  | R deriving (Show)

data Key = Wh WhiteKeyType
         | Bl deriving (Show)

keyboardLayout :: M.Map Int Keyboard.Key
keyboardLayout = M.fromList $ zip [0..] [ Wh L
                                        , Bl
                                        , Wh M
                                        , Bl
                                        , Wh R
                                        , Wh L
                                        , Bl
                                        , Wh M
                                        , Bl
                                        , Wh M
                                        , Bl
                                        , Wh R
                                        ]

blackKeySize = V2 16 76
whiteKeySize = V2 26 116
gap = 2

black = makeGColor 0.1 0.1 0.1
white = makeGColor 1 1 1
red = makeGColor 1 0.2 0.2
green = makeGColor 0 1 0
brightBlue = makeGColor 0.2 0.4 1
blueDark = makeGColor 0.7 0.8 1
blueDarker = makeGColor 0.2 0.25 0.5
  
drawKeyboard :: Keyboard -> Maybe Scale -> V2 G.GLfloat -> V2 G.GLfloat -> IO ()
drawKeyboard keyboard@(Keyboard keyMap) maybeScale origin (V2 w h) = do
  mapM_ drawKey keyData
  where
    keyData = zip4 keys (M.elems keyMap) keyTypes allowed
    keys = M.keys keyMap
    keyTypes = map (\k -> keyboardLayout ! (k `mod` 12)) keys
    allowed = maybe (repeat True) (\s -> map (inScale s . toNote) keys) maybeScale
    noOfWhites key = fromIntegral $ ((key `div` 12) * 7) + ([0,1,1,2,2,3,4,4,5,5,6,6] !! (key `mod` 12))
    drawKey :: (Int, PlayState, Key, Bool) -> IO ()
    drawKey (key, playState, keyType, allowed) = do
      case (keyType, (key == firstKey keyboard), (key == lastKey keyboard)) of
        (Wh L, _, False) -> drawWhiteKeyLeft
        (Wh L, _, True) -> drawWhiteKeyFull
        (Wh M, False, False) -> drawWhiteKeyMiddle
        (Wh M, True, _) -> drawWhiteKeyLeft
        (Wh M, _, True) -> drawWhiteKeyRight
        (Wh R, False, _) -> drawWhiteKeyRight
        (Wh R, True, _) -> drawWhiteKeyFull
        (Bl, _, _) -> drawBlackKey
      where
        keyboardWidth = ((fromIntegral $ M.size keyMap) * 7.0 / 12.0) * (x whiteKeySize + gap)
        keyboardHeight = y whiteKeySize
        keyboardScale = V2 (w / keyboardWidth) (h / keyboardHeight)
        keyColor = case (playState, allowed, keyType, maybeScale) of
                     (NotPlaying, True, Wh _, Just _) -> blueDark
                     (NotPlaying, True, _, Just _) -> blueDarker
                     (Playing, True, Wh _, _) -> brightBlue
                     (Playing, True, _, _) -> brightBlue
                     (Playing, _, _, _) -> red
                     (_, _, Wh _, _) -> white
                     (_, _, _, _) -> black
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