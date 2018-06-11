module AppState where

import Data.List ( delete )

import Music

data ScaleSelect = ScaleSelect { scale :: Maybe Scale
                               , availScales :: [Scale]
                               , chord :: Maybe Chord
                               , availChords :: [Chord]
                               , waitingForInput :: Bool
                               , inputNotes :: [Int]
                               , root :: Maybe Note
                               , actionKey :: Int
                               } deriving (Eq, Show)

data State = State { keyboard :: Keyboard
                   , scaleSelect :: ScaleSelect
                   } deriving (Eq, Show)

initState keyboard = State { keyboard = keyboard
                           , scaleSelect = ScaleSelect { scale = Nothing
                                                       , availScales = []
                                                       , chord = Nothing 
                                                       , availChords = []
                                                       , waitingForInput = False
                                                       , inputNotes = []
                                                       , root = Nothing
                                                       , actionKey = firstKey keyboard
                                                       }
                           }

clear :: ScaleSelect -> ScaleSelect
clear ss = ss { scale = Nothing
              , chord = Nothing
              , inputNotes = []
              , root = Nothing
              , availScales = []
              , availChords = []
              }

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

data ChordMap = ChordMap [Note] [(Note, [(Integer, Integer)])] deriving (Show, Eq)

chordMap :: Keyboard -> ChordMap
chordMap keyboard = 
  let scales = map (\(Scale r t) -> r) $ filter (\(Scale r t) -> case t of
                                                                   Major -> True
                                                                   _ -> False) $ scalesFor $ notesPlaying keyboard
      chords = keyByValue . indexify2 $ map (scaleCycle 1 (-3)) (scaleCycle 1 (-5) C)
  in  (ChordMap scales chords)