module Keyboard (
  Keyboard (..),
  makeKeyboard,
  keyDown,
  keyUp,
  notesPlaying,
  drawKeyboard,
  rotateListL,
  rotateListR
) where

import qualified Graphics.UI.GLUT as G
import Data.List (zip4, delete)
import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))

import Music
import Graphics

data Keyboard = Keyboard { firstKey :: Int
                         , lastKey :: Int
                         , keysPlaying :: [Int]
                         } deriving (Show, Eq)

makeKeyboard :: Int -> Int -> Keyboard
makeKeyboard start end = Keyboard start end []

keyDown :: Int -> Keyboard -> Keyboard
keyDown key kbd | key >= firstKey kbd
                  && key <= lastKey kbd 
                  && not (key `elem` keysPlaying kbd) = kbd { keysPlaying = key : keysPlaying kbd }
                | otherwise = kbd

keyUp :: Int -> Keyboard -> Keyboard
keyUp key kbd = kbd { keysPlaying = delete key $ keysPlaying kbd }

notesPlaying :: Keyboard -> [Note]
notesPlaying kbd = map toNote $ keysPlaying kbd

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

rotateListL :: [a] -> Int -> [a]
rotateListL [] _ = []
rotateListL xs 0 = xs 
rotateListL (x:xs) n = rotateListL (xs ++ [x]) (n - 1)
  
rotateListR :: [a] -> Int -> [a]
rotateListR [] _ = []
rotateListR xs 0 = xs 
rotateListR xs n = rotateListL (last xs : init xs) (n - 1)

drawKeyboard :: Keyboard -> Maybe Scale -> V2 G.GLfloat -> V2 G.GLfloat -> IO ()
drawKeyboard kbd maybeScale origin (V2 w h) = do
  mapM_ drawKey keyData
  where
    keys = take (lastKey kbd - firstKey kbd + 1) . (drop $ firstKey kbd) $ [0..]
    playing = map (`elem` keysPlaying kbd) $ keys
    keyData = zip4 keys playing keyTypes allowed
    keyTypes = map (\k -> keyboardLayout ! (k `mod` 12)) keys
    allowed = maybe (repeat True) (\s -> map (inScale s . toNote) keys) maybeScale
    noOfWhites' k = fromIntegral $ ((k `div` 12) * 7) + ([0,1,1,2,2,3,4,4,5,5,6,6] !! (k `mod` 12))
    noOfWhites key = (noOfWhites' key) - (noOfWhites' $ firstKey kbd)
    drawKey :: (Int, Bool, Key, Bool) -> IO ()
    drawKey (key, playing, keyType, allowed) = do
      case (keyType, (key == firstKey kbd), (key == lastKey kbd)) of
        (Wh L, _, False) -> drawWhiteKeyLeft
        (Wh L, _, True) -> drawWhiteKeyFull
        (Wh M, False, False) -> drawWhiteKeyMiddle
        (Wh M, True, _) -> drawWhiteKeyLeft
        (Wh M, _, True) -> drawWhiteKeyRight
        (Wh R, False, _) -> drawWhiteKeyRight
        (Wh R, True, _) -> drawWhiteKeyFull
        (Bl, _, _) -> drawBlackKey
      where
        keyboardWidth = ((fromIntegral $ length keys) * 7.0 / 12.0) * (x whiteKeySize + gap)
        keyboardHeight = y whiteKeySize
        keyboardScale = V2 (w / keyboardWidth) (h / keyboardHeight)
        keyColor = case (playing, allowed, keyType, maybeScale) of
                     (False, True, Wh _, Just _) -> blueDark
                     (False, True, _, Just _) -> blueDarker
                     (True, True, Wh _, _) -> brightBlue
                     (True, True, _, _) -> brightBlue
                     (True, _, _, _) -> red
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