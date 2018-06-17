module UI (
  UIEvent(..),
  UIAction(..),
  setupUI,
  handleUIAction,
  startUI,
  exitUI
) where

import Graphics.UI.GLUT hiding ( R, scale )
import qualified Graphics.UI.GLUT as GL ( scale )
import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))
import Data.List ( zip4, intercalate )

import AppState
import Graphics
import Music

absoluteWidth = 1920
absoluteHeight = 1080

black = makeGColor 0.25 0.25 0.25
white = makeGColor 1 1 1
red = makeGColor 1 0.2 0.2
redLighter = makeGColor 0.6 0.3 0.3
redDark = makeGColor 0.5 0.12 0.1
redDarker = makeGColor 0.5 0.12 0.1
green = makeGColor 0 1 0
brightBlue = makeGColor 0.2 0.4 1
blueDark = makeGColor 0.7 0.8 1
blueDarker = makeGColor 0.5 0.6 0.8
outline = makeGColor 0 0 0
background = makeGColor 0.1 0.1 0.1

data UIEvent = UIKeyDown Char
             | UIKeyUp Char
             | UIRequestRefresh
             | UIRequestReshape Size deriving (Show)

data UIAction = UIRefresh
              | UIReshape Size
              | UIExit deriving (Show)

exitUI :: IO ()
exitUI = leaveMainLoop

startUI :: IO ()
startUI = mainLoop

setupUI handler = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Musix"
  actionOnWindowClose $= ContinueExecution
  fullScreen
  idleCallback $= Just (postRedisplay Nothing)
  displayCallback $= handler UIRequestRefresh
  reshapeCallback $= Just (handler . UIRequestReshape)
  let keyboardMouse (Char c) Down _ _ = handler $ UIKeyDown c
      keyboardMouse (Char c) Up _ _ = handler $ UIKeyUp c
      keyboardMouse _ _ _ _ = return ()
  keyboardMouseCallback $= Just keyboardMouse

handleUIAction :: State -> UIAction -> IO Bool
handleUIAction state UIRefresh = render state >> return False
handleUIAction state (UIReshape size) = reshape size >> render state >> return False
handleUIAction state UIExit = render state >> return True

reshape :: Size -> IO ()
reshape screenSize = do
  viewport $= viewportSize screenSize absoluteSize
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral absoluteWidth) (fromIntegral absoluteHeight) 0
  where
  absoluteSize = Size absoluteWidth absoluteHeight
  aspectRatio = fromIntegral absoluteWidth / fromIntegral absoluteHeight
  viewportSize :: Size -> Size -> (Position, Size)
  viewportSize (Size screenW screenH) (Size absW absH) =
    (Position (round x) (round y), Size (round w) (round h))
    where 
      screenAspectRatio = fromIntegral screenW / fromIntegral screenH
      scale = if screenAspectRatio > aspectRatio then
                fromIntegral screenH / fromIntegral absoluteHeight
              else
                fromIntegral screenW / fromIntegral absoluteWidth
      w = fromIntegral absoluteWidth * scale
      h = fromIntegral absoluteHeight * scale
      x = max 0 (fromIntegral screenW - w) / 2
      y = max 0 (fromIntegral screenH - h) / 2

render :: State -> IO ()
render state = do
  clearScreen background
  drawKeyboard state (V2 10 100) 1900
  drawUIText state (V2 100 400)
  flush

data WhiteKeyType = L
                  | M
                  | R deriving (Show)

data Key = Wh WhiteKeyType
         | Bl deriving (Show)

drawKeyboard :: State -> V2 GLfloat -> GLfloat -> IO ()
drawKeyboard state origin w = do
  mapM_ drawKey keys
  drawText white (origin |+| (V2 10 (-50))) $ deviceName kbd
  where
  kbd = keyboard state
  maybeScale = (scale . scaleSelect) state
  keys = zip4 keys' playing keyTypes allowed
    where
    keys' = take (lastKey kbd - firstKey kbd + 1) . (drop $ firstKey kbd) $ [0..]
    playing = map (`elem` keysPlaying kbd) $ keys'
    keyTypes = map (\k -> keyboardLayout ! (k `mod` 12)) keys'
    allowed = maybe (repeat True) (\s -> map (inScale s . toNote) keys') maybeScale
    keyboardLayout = M.fromList $ zip [0..] [Wh L, Bl, Wh M, Bl, Wh R, Wh L, Bl, Wh M, Bl, Wh M, Bl, Wh R]
  
  noOfWhites key = (noOfWhites' key) - (noOfWhites' $ firstKey kbd)
    where
    noOfWhites' k = fromIntegral $ ((k `div` 12) * 7) + ([0,1,1,2,2,3,4,4,5,5,6,6] !! (k `mod` 12))
  
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
    keyColor = case (playing, allowed, keyType, maybeScale, colourAllowedNotes state) of
                 (False, True, Wh _, Just _, True) -> blueDarker
                 (False, True, _, Just _, True) -> blueDarker
                 (True, True, _, _, _) -> brightBlue
                 (True, _, _, _, _) -> red
                 (_, _, Wh _, _, _) -> white
                 _ -> black
    drawWhiteKeyLeft = do
      drawWhiteBase
      drawRect' outline (whiteKeyOffset |+| V2 (-gap) (-gap)) (V2 0 0) (V2 (x whiteKeySize - x blackKeyInset) (y blackKeySize) |+| V2 (gap * 2) gap)
      drawRect' keyColor whiteKeyOffset (V2 0 0) (V2 (x whiteKeySize - x blackKeyInset) (y blackKeySize))
    drawWhiteKeyMiddle = do
      drawWhiteBase
      drawRect' outline (whiteKeyOffset |+| V2 (-gap) (-gap)) (V2 (x blackKeyInset) 0) (V2 (x whiteKeySize - x blackKeyInset) (y blackKeySize) |+| V2 (gap * 2) gap)
      drawRect' keyColor whiteKeyOffset (V2 (x blackKeyInset) 0) (V2 (x whiteKeySize - x blackKeyInset) (y blackKeySize))
    drawWhiteKeyRight = do
      drawWhiteBase
      drawRect' outline (whiteKeyOffset |+| V2 (-gap) (-gap)) (V2 (x blackKeyInset) 0) (V2 (x whiteKeySize) (y blackKeySize) |+| V2 (gap * 2) gap)
      drawRect' keyColor whiteKeyOffset (V2 (x blackKeyInset) 0) (V2 (x whiteKeySize) (y blackKeySize))
    drawWhiteKeyFull = do
      drawRect' outline (whiteKeyOffset |+| V2 (-gap) (-gap)) (V2 0 0) (whiteKeySize |+| V2 (gap * 2) (gap * 2))
      drawRect' keyColor whiteKeyOffset (V2 0 0) whiteKeySize
    drawBlackKey = do
      drawRect' outline (blackKeyOffset |+| V2 (-gap) (-gap)) (V2 0 0) (blackKeySize |+| (V2 gap gap |*| 2))
      drawRect' keyColor blackKeyOffset (V2 0 0) blackKeySize
    drawWhiteBase = do
      drawRect' outline (whiteKeyOffset |+| V2 0 (y blackKeySize) |+| V2 (-gap) 0) (V2 0 0) (V2 (x whiteKeySize) (y blackKeyInset) |+| (V2 (gap * 2) gap))
      drawRect' keyColor (whiteKeyOffset |+| V2 0 (y blackKeySize)) (V2 0 0) (V2 (x whiteKeySize) (y blackKeyInset))
    whiteKeyOffset = V2 (noOfWhites key * (x whiteKeySize + gap)) 0
    blackKeyOffset = whiteKeyOffset |+| V2 (-(x blackKeyInset)) (-gap)
    drawRect' col offset p1 p2 =
      drawRect col (origin |+| (V2 offsetX 0) |+| (offset |**| keyboardScale)) (p1 |**| keyboardScale) (p2 |**| keyboardScale)
  
  blackKeySize = V2 16 76
  whiteKeySize = V2 26 116
  gap = 2
  keyboardWidth = ((fromIntegral $ length keys) * 7.0 / 12.0) * (x whiteKeySize + gap)
  keyboardHeight = y whiteKeySize
  sc = min (w / keyboardWidth) (((w / 1900) * 180) / keyboardHeight)
  offsetX = max 0 ((w - (sc * keyboardWidth)) / 2)
  keyboardScale = V2 sc sc
  blackKeyInset = V2 ((x blackKeySize + gap) / 2) (y whiteKeySize - y blackKeySize)

drawUIText :: State -> V2 GLfloat -> IO ()
drawUIText state origin = do
  drawText white origin $ "Notes: " ++ (intercalate " " $ map show $ notesPlaying $ keyboard state)
  drawText white (origin |+| V2 0 50) $ "Chord: " ++ (drawChordText $ scaleSelect state)
  drawText white (origin |+| V2 0 100) $ "Scale: " ++ (drawScaleText $ scaleSelect state)
  where
  drawChordText :: ScaleSelect -> String
  drawChordText ScaleSelect { chord = Just sc } = show sc
  drawChordText ScaleSelect { chord = Nothing, parsing = True, root = Nothing } = "waiting for chord..."
  drawChordText ScaleSelect { chord = Nothing, parsing = True, availChords = [], root = Just r } = show r ++ " ?"
  drawChordText ScaleSelect { chord = Nothing, parsing = True, availChords = cs } = intercalate " / " $ map show cs
  drawChordText _ = ""

  drawScaleText :: ScaleSelect -> String
  drawScaleText ScaleSelect { scale = Just sc, root = Just r } = showInKey r sc
  drawScaleText ScaleSelect { scale = Nothing, parsing = True, root = Nothing } = "waiting for scale..."
  drawScaleText ScaleSelect { scale = Nothing, parsing = True, availScales = [] } = "?"
  drawScaleText ScaleSelect { scale = Nothing, parsing = True, availScales = scs, root = Just r } = intercalate " / " $ map (showInKey r) scs
  drawScaleText _ = ""