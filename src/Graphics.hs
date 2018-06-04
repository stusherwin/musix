module Graphics (
  Drawable,
  drawCircle,
  drawPoint,
  drawPolygon,
  drawRect,
  V2(V2),
  (|+|),
  (|-|),
  (|*|),
  (|/|),
  x,
  y,
  draw,
  GPolygon(GPolygon),
  GRect(GRect),
  GColor,
  makeGColor, 
  clearScreen
) where

import Graphics.UI.GLUT

newtype GColor = GColor { getColor3 :: Color3 GLfloat }
makeGColor :: GLfloat -> GLfloat -> GLfloat -> GColor
makeGColor r g b = GColor { getColor3 = Color3 r g b }

data V2 a = V2 a a deriving (Show)
V2 x1 y1 |+| V2 x2 y2 = V2 (x1 + x2) (y1 + y2)
V2 x1 y1 |-| V2 x2 y2 = V2 (x1 - x2) (y1 - y2)
V2 x y |*| a = V2 (x * a) (y * a)
V2 x y |/| a = V2 (x / a) (y / a)
x (V2 x _) = x
y (V2 _ y) = y

class Drawable a where
  draw :: a -> V2 GLfloat -> IO ()

data GPolygon = GPolygon GColor [V2 GLfloat]
instance Drawable GPolygon where
  draw (GPolygon col vertices) origin = drawPolygon col origin vertices

data GRect = GRect GColor (V2 GLfloat) (V2 GLfloat)
instance Drawable GRect where
  draw (GRect col v0 v1) origin = drawRect col origin v0 v1

drawCircle :: Double -> Double -> GColor -> V2 GLfloat -> IO ()
drawCircle res r col (V2 x y) = do
  let vertices = [let x = r*sin (2*pi*k/res); y = r*cos (2*pi*k/res) in V2 (realToFrac x) (realToFrac y) | k <- [1..res]]
  let position = V2 (realToFrac x) (realToFrac y)
  drawPolygon col position vertices

drawPoint :: GColor -> V2 GLfloat -> IO ()
drawPoint col (V2 x y) = do
  preservingMatrix $ do
    translate $ Vector3 x y 0
    color $ getColor3 col
    renderPrimitive Points $ do
      vertex $ Vertex3 0 0 (0 :: GLfloat)

drawPolygon :: GColor -> V2 GLfloat -> [V2 GLfloat] -> IO ()
drawPolygon col (V2 xo yo) vs = do
  preservingMatrix $ do
    translate $ Vector3 xo yo 0
    color $ getColor3 col
    renderPrimitive Polygon $ do
      mapM_ (\(V2 x y) -> vertex $ Vertex3 x y 0) vs

drawRect :: GColor -> V2 GLfloat -> V2 GLfloat -> V2 GLfloat -> IO ()
drawRect col orig (V2 x0 y0) (V2 x1 y1) = do
  drawPolygon col orig [ V2 x0 y0, V2 x0 y1, V2 x1 y1, V2 x1 y0 ]

clearScreen :: IO ()
clearScreen = clear [ ColorBuffer ]