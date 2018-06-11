module AppState where

import Data.List ( delete )

import Music

data Keyboard = Keyboard { firstKey :: Int
                         , lastKey :: Int
                         , keysPlaying :: [Int]
                         } deriving (Show, Eq)

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

clearSS :: ScaleSelect -> ScaleSelect
clearSS ss = ss { scale = Nothing
              , chord = Nothing
              , inputNotes = []
              , root = Nothing
              , availScales = []
              , availChords = []
              }

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