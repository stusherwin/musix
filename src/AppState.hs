module AppState where

import Data.List ( delete, sort )

import Music

data Keyboard = Keyboard { firstKey :: Int
                         , lastKey :: Int
                         , keysPlaying :: [Int]
                         , deviceName :: String
                         } deriving (Show, Eq)

data ScaleSelect = ScaleSelect { scale :: Maybe Scale
                               , availScales :: [Scale]
                               , chord :: Maybe Chord
                               , availChords :: [Chord]
                               , parsing :: Bool
                               , root :: Maybe Note
                               , actionKey :: Int
                               } deriving (Eq, Show)

data State = State { keyboard :: Keyboard
                   , scaleSelect :: ScaleSelect
                   , colourAllowedNotes :: Bool
                   } deriving (Eq, Show)

initState keyboard = State { keyboard = keyboard
                           , colourAllowedNotes = False
                           , scaleSelect = ScaleSelect { scale = Just (Scale Eb Major) --Nothing
                                                       , availScales = []
                                                       , chord = Nothing 
                                                       , availChords = []
                                                       , parsing = False
                                                       , root = Just Eb --Nothing
                                                       , actionKey = firstKey keyboard
                                                       }
                           }

clearSS :: ScaleSelect -> ScaleSelect
clearSS ss = ss { scale = Nothing
              , chord = Nothing
              , root = Nothing
              , availScales = []
              , availChords = []
              }

makeKeyboard :: String -> Int -> Int -> Keyboard
makeKeyboard n start end = Keyboard start end [] n

keyDown :: Int -> Keyboard -> Keyboard
keyDown key kbd | key >= firstKey kbd
                  && key <= lastKey kbd 
                  && not (key `elem` keysPlaying kbd) = kbd { keysPlaying = sort $ key : keysPlaying kbd }
                | otherwise = kbd

keyUp :: Int -> Keyboard -> Keyboard
keyUp key kbd = kbd { keysPlaying = delete key $ keysPlaying kbd }

notesPlaying :: Keyboard -> [Note]
notesPlaying kbd = map toNote $ keysPlaying kbd