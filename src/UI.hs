module UI (
  UIAction(..),
  setupUI,
  handleUiAction,
  initUI,
  enterEventLoop,
  exitEventLoop
) where

import Graphics.UI.GLUT hiding ( R, scale )
import qualified Graphics.UI.GLUT as GL ( scale )
import qualified Data.Map.Lazy as M
import Data.Map.Lazy ((!))
import Data.List ( zip4, intercalate )

import AppState
import Graphics
import Music

data UIAction = UIKeyDown Char
              | UIKeyUp Char
              | UIRefresh
              | UIReshape Size deriving (Show)

absoluteWidth = 1920
absoluteHeight = 1080
absoluteSize = Size absoluteWidth absoluteHeight
aspectRatio = fromIntegral absoluteWidth / fromIntegral absoluteHeight

initUI :: IO ()
initUI = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Musix"
  actionOnWindowClose $= ContinueExecution
  fullScreen

enterEventLoop :: IO ()
enterEventLoop = mainLoop

exitEventLoop :: IO ()
exitEventLoop = leaveMainLoop

setupUI handler = do
  idleCallback $= Just (postRedisplay Nothing)
  displayCallback $= handler UIRefresh
  reshapeCallback $= Just (handler . UIReshape)
  let keyboardMouse (Char c) Down _ _ = handler $ UIKeyDown c
      keyboardMouse (Char c) Up _ _ = handler $ UIKeyUp c
      keyboardMouse _ _ _ _ = return ()
  keyboardMouseCallback $= Just keyboardMouse

handleUiAction :: State -> UIAction -> IO Bool
handleUiAction state (UIReshape size) = reshape size >> render state >> return False
handleUiAction state (UIKeyDown '\27') = render state >> return True
handleUiAction state UIRefresh = render state >> return False
handleUiAction _ _ = return False

render :: State -> IO ()
render state = do
  clearScreen
  drawKeyboard (keyboard state) (scale $ scaleSelect state) (V2 10 50) (V2 1900 180)
  drawUIText state
  --draw (chordMap keyboard) (V2 100 400) (V2 0 0)
  flush

reshape :: Size -> IO ()
reshape screenSize = do
  viewport $= viewportSize screenSize absoluteSize
  matrixMode $= Projection
  loadIdentity
  ortho2D 0 (fromIntegral absoluteWidth) (fromIntegral absoluteHeight) 0

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

data WhiteKeyType = L
                  | M
                  | R deriving (Show)

data Key = Wh WhiteKeyType
         | Bl deriving (Show)

keyboardLayout :: M.Map Int UI.Key
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

drawUIText :: State -> IO ()
drawUIText state = do
  drawText (makeGColor 1 1 1) (V2 100 400) $ "Notes: " ++ (intercalate " " $ map show $ notesPlaying $ keyboard state)
  drawText (makeGColor 1 1 1) (V2 100 500) $ "Chord: " ++ (drawChordText $ scaleSelect state)
  drawText (makeGColor 1 1 1) (V2 100 600) $ "Scale: " ++ (drawScaleText $ scaleSelect state)
  where
  drawChordText :: ScaleSelect -> String
  drawChordText ScaleSelect { chord = Just sc } = show sc
  drawChordText ScaleSelect { chord = Nothing, waitingForInput = True, root = Nothing } = "waiting for chord..."
  drawChordText ScaleSelect { chord = Nothing, waitingForInput = True, availChords = [], root = Just r } = show r ++ " ?"
  drawChordText ScaleSelect { chord = Nothing, waitingForInput = True, availChords = cs } = intercalate " / " $ map show cs
  drawChordText _ = "none"

  drawScaleText :: ScaleSelect -> String
  drawScaleText ScaleSelect { scale = Just sc, root = Just r } = showInKey r sc
  drawScaleText ScaleSelect { scale = Nothing, waitingForInput = True, root = Nothing } = "waiting for scale..."
  drawScaleText ScaleSelect { scale = Nothing, waitingForInput = True, availScales = [] } = "?"
  drawScaleText ScaleSelect { scale = Nothing, waitingForInput = True, availScales = scs, root = Just r } = intercalate " / " $ map (showInKey r) scs
  drawScaleText _ = "none"

drawKeyboard :: Keyboard -> Maybe Scale -> V2 GLfloat -> V2 GLfloat -> IO ()
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
    drawKey :: (Int, Bool, UI.Key, Bool) -> IO ()
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