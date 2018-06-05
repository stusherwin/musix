module ChordMap (
  ChordMap(..),
  chordMap
) where

import Graphics.UI.GLUT 
import Graphics
import Music
import Keyboard

data ChordMap = ChordMap [Note] [(Note, [(Integer, Integer)])] deriving (Show, Eq)

chordMap :: Keyboard -> ChordMap
chordMap keyboard = 
  let scales = map (\(Scale r t) -> r) $ filter (\(Scale r t) -> case t of
                                                                   Major -> True
                                                                   _ -> False) $ scalesFor $ notesPlaying keyboard
      chords = keyByValue . indexify2 $ map (scaleCycle 1 (-3)) (scaleCycle 1 (-5) C)
  in  (ChordMap scales chords)

drawChordmap (ChordMap scales chords) (V2 xo yo) size = do
  preservingMatrix $ do
      translate $ Vector3 xo yo (0 :: GLfloat)
      mapM_ (\(c, coords) ->
        mapM_ (\(x, y) -> preservingMatrix $ do
        translate $ Vector3 (fromInteger x*150) (fromInteger y*150) (0 :: GLfloat)
        rotate 180 $ Vector3 1 0 (0 ::  GLfloat)
        translate $ Vector3 0 (-100.0) (0 :: GLfloat)
        scale 0.25 0.25 (1::GLfloat)
        let col = case c `elem` scales of
                    True -> makeGColor 1 0 0
                    _ -> makeGColor 0.3 0.3 0.3
        drawCircle (20.0) (200.0) col (V2 0 0)
        translate $ Vector3 (-40) (-40) (0 :: GLfloat)
        color $ Color3 1 1 (1 :: GLfloat)
        renderString Roman (show c)) coords) chords