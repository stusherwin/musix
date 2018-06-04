module UI (
  UIAction(..),
  setupUI,
  reshape
) where

import Graphics.UI.GLUT 

data UIAction = UIKeyDown Char
              | UIKeyUp Char
              | UIRefresh
              | UIReshape Size deriving (Show)

absoluteWidth = 1920
absoluteHeight = 1080
absoluteSize = Size absoluteWidth absoluteHeight
aspectRatio = fromIntegral absoluteWidth / fromIntegral absoluteHeight

setupUI handler = do
  idleCallback $= Just (postRedisplay Nothing)
  displayCallback $= handler UIRefresh
  reshapeCallback $= Just (handler . UIReshape)
  let keyboardMouse (Char c) Down _ _ = handler $ UIKeyDown c
      keyboardMouse (Char c) Up _ _ = handler $ UIKeyUp c
      keyboardMouse _ _ _ _ = return ()
  keyboardMouseCallback $= Just keyboardMouse

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