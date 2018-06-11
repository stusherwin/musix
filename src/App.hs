{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}

module App where

import Control.Arrow ( returnA )
import FRP.Yampa ( Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>), mapFilterE )
import Data.List ( elemIndex, intercalate, find, delete, sort, (\\), nub )
import Data.Map.Lazy ( (!) )
import Data.Maybe ( listToMaybe )

import AppState
import Midi
import Music
import UI

data EventType = UI UIAction
               | Midi MidiEvent deriving (Show) 

getUIAction :: EventType -> Maybe UIAction
getUIAction (UI uiAction) = Just uiAction
getUIAction _ = Nothing

getMidi :: EventType -> Maybe MidiEvent
getMidi (Midi midi) = Just midi
getMidi _ = Nothing

mainSF :: SF (Event EventType, State) (Event (IO Bool), State)
mainSF = proc (event, state) -> do
  let uiAction = (mapFilterE getUIAction) event
  vmidi <- virtualMidi -< uiAction
  let midi = rMerge vmidi (mapFilterE getMidi $ event)
  keyboard' <- pressKey -< (midi, keyboard state)
  scaleSelect' <- changeScale -< (keyboard', scaleSelect state, midi)

  let state' = state { keyboard = keyboard'
                     , scaleSelect = scaleSelect'
                     }

  let io = handleUiAction state' <$> uiAction

  returnA -< (io, state')

virtualMidi :: SF (Event UIAction) (Event MidiEvent)
virtualMidi = proc uiAction -> do
  returnA -< case uiAction of
               Event (UIKeyDown c) -> toEvent $ NoteOn <$> toNote c
               Event (UIKeyUp c)   -> toEvent $ NoteOff <$> toNote c
               _ -> NoEvent
  where
    keys = ['z', 's', 'x', 'd', 'c', 'v', 'g', 'b', 'h', 'n', 'j', 'm',
            'q', '2', 'w', '3', 'e', 'r', '5', 't', '6', 'y', '7', 'u']
    toNote c = c `elemIndex` keys
    toEvent = maybe NoEvent Event

pressKey :: SF (Event MidiEvent, Keyboard) Keyboard
pressKey = proc (event, keyboard) -> do
  returnA -< case event of
               Event (NoteOn n)  -> keyDown n keyboard
               Event (NoteOff n) -> keyUp n keyboard
               _ -> keyboard

changeScale :: SF (Keyboard, ScaleSelect, Event MidiEvent) ScaleSelect
changeScale = proc input -> do
  shouldSwitch <- decision -< input
  output <- rSwitch passThrough -< (input, shouldSwitch)
  returnA -< output

  where
  passThrough :: SF (Keyboard, ScaleSelect, Event MidiEvent) ScaleSelect
  passThrough = arr $ \(_, ss, _) -> ss

  decision :: SF (Keyboard, ScaleSelect, Event MidiEvent) (Event (SF (Keyboard, ScaleSelect, Event MidiEvent) ScaleSelect))
  decision = proc (kbd, ss, midi) -> do
    returnA -< case (ss, midi) of
                 (ScaleSelect { parsing = False, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clearSS ss { parsing = True } -:> parse
                 (ScaleSelect { parsing = True, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clearSS ss { parsing = False } -:> passThrough
                 (ScaleSelect { parsing = True, scale = Just _ }, _)
                   -> Event $ ss { parsing = False } -:> passThrough
                 _ -> NoEvent

  parse :: SF (Keyboard, ScaleSelect, Event MidiEvent) ScaleSelect
  parse = proc (kbd, ss, midi) -> do
    ssWithRoot <- parseRoot -< (kbd, ss)
    ssWithChord <- parseChord -< (kbd, ssWithRoot)
    ssWithScale <- parseScale -< (kbd, ssWithChord)
    returnA -< ssWithScale
  
  parseRoot :: SF (Keyboard, ScaleSelect) ScaleSelect
  parseRoot = proc (kbd, ss) -> do
    returnA -< case listToMaybe $ keysPlaying kbd of
                 Just r | r /= actionKey ss -> ss { root = Just $ toNote r }
                 _ -> ss { root = Nothing }

  parseChord :: SF (Keyboard, ScaleSelect) ScaleSelect
  parseChord = proc (kbd, ss) -> do
    let cs = case root ss of
               Just r -> chordsForRoot r $ notesPlaying kbd
               _ -> []
    returnA -< ss { availChords = cs
                  , chord = case cs of
                              [c] -> Just c
                              _ -> Nothing
                  }

  parseScale :: SF (Keyboard, ScaleSelect) ScaleSelect
  parseScale = proc (kbd, ss) -> do
    let scs = let chordScales = nub [ s | c <- availChords ss, s <- map fst $ scalesForChord c ]
              in  filter (\s -> (notesPlaying kbd) `containedIn` (scaleNotes s)) chordScales
    returnA -< ss { availScales = scs
                  , scale = case scs of
                              [sc] -> Just sc
                              _ -> Nothing
                  }