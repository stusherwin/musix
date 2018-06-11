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
                 (ScaleSelect { waitingForInput = False, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clearSS ss { waitingForInput = True } -:> waitForInput
                 (ScaleSelect { waitingForInput = True, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clearSS ss { waitingForInput = False } -:> passThrough
                 (ScaleSelect { waitingForInput = True, scale = Just _ }, _)
                   -> Event $ ss { waitingForInput = False } -:> passThrough
                 _ -> NoEvent

  waitForInput :: SF (Keyboard, ScaleSelect, Event MidiEvent) ScaleSelect
  waitForInput = proc (kbd, ss, midi) -> do
    ssWithRoot <- findRoot -< (kbd, ss)
    ssWithChord <- waitForChord -< (kbd, ssWithRoot)
    ssWithScale <- waitForScale -< (kbd, ssWithChord)
    returnA -< ssWithScale
  
  findRoot :: SF (Keyboard, ScaleSelect) ScaleSelect
  findRoot = proc (kbd, ss) -> do
    returnA -< case listToMaybe $ keysPlaying kbd of
                 Just r | r /= actionKey ss -> ss { root = Just $ toNote r }
                 _ -> ss { root = Nothing }

  waitForChord :: SF (Keyboard, ScaleSelect) ScaleSelect
  waitForChord = proc (kbd, ss) -> do
    let cs = case root ss of
               Just r -> chordsForRoot r $ notesPlaying kbd
               _ -> []
    returnA -< ss { availChords = cs
                  , chord = case cs of
                              [c] -> Just c
                              _ -> Nothing
                  }

  waitForScale :: SF (Keyboard, ScaleSelect) ScaleSelect
  waitForScale = proc (kbd, ss) -> do
    let scs = let chordScales = nub [ s | c <- availChords ss, s <- map fst $ scalesForChord c ]
                  notes = notesPlaying kbd
              in  filter (\s -> notes `containedIn` (scaleNotes s)) chordScales
    returnA -< ss { availScales = scs
                  , scale = case scs of
                              [sc] -> Just sc
                              _ -> Nothing
                  }