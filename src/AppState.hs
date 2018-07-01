module AppState where

import Data.List ( delete, sort )

import Music
import Midi

data Keyboard = Keyboard { firstKey :: Int
                         , lastKey :: Int
                         , keysPlaying :: [Int]
                         } deriving (Show, Eq)

data ScaleSelect = ScaleSelect { scale :: Maybe Scale
                               , availScales :: [Scale]
                               , chord :: Maybe Chord
                               , availChords :: [Chord]
                               , parsing :: Bool
                               , root :: Maybe Note
                               } deriving (Eq, Show)

data State = State { keyboard :: Keyboard
                   , midiSources :: [MidiSourceInfo]
                   , scaleSelect :: ScaleSelect
                   , colourAllowedNotes :: Bool
                   } deriving (Eq, Show)

initState keyboard = State { keyboard = keyboard
                           , midiSources = []
                           , colourAllowedNotes = False
                           , scaleSelect = ScaleSelect { scale = Just (Scale Eb Major) --Nothing
                                                       , availScales = []
                                                       , chord = Nothing 
                                                       , availChords = []
                                                       , parsing = False
                                                       , root = Just Eb --Nothing
                                                       }
                           }

clearSS :: ScaleSelect -> ScaleSelect
clearSS ss = ss { scale = Nothing
              , chord = Nothing
              , root = Nothing
              , availScales = []
              , availChords = []
              }

makeKeyboard :: Int -> Int -> Keyboard
makeKeyboard start end = Keyboard start end []

keyDown :: Int -> Keyboard -> Keyboard
keyDown key kbd | key >= firstKey kbd
                  && key <= lastKey kbd 
                  && not (key `elem` keysPlaying kbd) = kbd { keysPlaying = sort $ key : keysPlaying kbd }
                | otherwise = kbd

keyUp :: Int -> Keyboard -> Keyboard
keyUp key kbd = kbd { keysPlaying = delete key $ keysPlaying kbd }

notesPlaying :: Keyboard -> [Note]
notesPlaying kbd = map toNote $ keysPlaying kbd