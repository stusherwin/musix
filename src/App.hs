{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}

module App where

import Control.Arrow ( returnA )
import FRP.Yampa ( Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>) )
import Data.List ( elemIndex, intercalate, find, delete, sort, (\\), nub )
import Data.Map.Lazy ( (!) )
import Data.Maybe ( listToMaybe )

import AppState
import Midi
import Music
import UI

data EventType = UI UIAction
               | Midi MidiEvent deriving (Show) 

mainSF :: SF (Event EventType, State) (Event (IO Bool), State)
mainSF = proc (event, state) -> do
  let uiAction = getUIAction event
  vmidi <- virtualMidi -< uiAction
  let midi = rMerge vmidi (getMidi event)
  keyboard' <- pressKey -< (midi, keyboard state)
  scaleSelect' <- changeScale -< (scaleSelect state, midi)

  let state' = state { keyboard = keyboard'
                     , scaleSelect = scaleSelect'
                     }

  let io = handleUiAction state' <$> uiAction

  returnA -< (io, state')

getUIAction :: Event EventType -> Event UIAction
getUIAction (Event (UI uiAction)) = Event uiAction
getUIAction _ = NoEvent

getMidi :: Event EventType -> Event MidiEvent
getMidi (Event (Midi midi)) = Event midi
getMidi _ = NoEvent

changeScale :: SF (ScaleSelect, Event MidiEvent) ScaleSelect
changeScale = proc input -> do
  shouldSwitch <- decision -< input
  output <- rSwitch (arr fst) -< (input, shouldSwitch)
  returnA -< output

  where
  decision :: SF (ScaleSelect, Event MidiEvent) (Event (SF (ScaleSelect, Event MidiEvent) ScaleSelect))
  decision = proc (ss, midi) -> do
    returnA -< case (ss, midi) of
                 (ScaleSelect { waitingForInput = False, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clear ss { waitingForInput = True } -:> waitForInput
                 (ScaleSelect { waitingForInput = True, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clear ss { waitingForInput = False } -:> arr fst
                 (ScaleSelect { waitingForInput = True, scale = Just _ }, _)
                   -> Event $ ss { waitingForInput = False } -:> arr fst
                 _ -> NoEvent
    where
    playingAllNotes :: [Note] -> [Note] -> Bool
    playingAllNotes shouldBePlaying test = null $ shouldBePlaying \\ test

  waitForInput :: SF (ScaleSelect, Event MidiEvent) ScaleSelect
  waitForInput = proc (ss, midi) -> do
    ss' <- notesPlaying -< (ss, midi)
    ssWithChord <- waitForChord -< ss'
    ssWithScale <- waitForScale -< ssWithChord
    returnA -< ssWithScale
  
  notesPlaying :: SF (ScaleSelect, Event MidiEvent) ScaleSelect
  notesPlaying = proc (ss, midi) -> do
    let ns = inputNotes ss
    let ns' = case midi of
                Event (NoteOn n) -> if n `elem` ns then ns else sort $ n : ns
                Event (NoteOff n) -> delete n ns
                _ -> ns
    returnA -< ss { inputNotes = ns', root = toNote <$> listToMaybe ns' }

  waitForChord :: SF ScaleSelect ScaleSelect
  waitForChord = proc ss -> do
    let cs = case root ss of
               Just r -> chordsForRoot r $ map toNote $ inputNotes ss
               _ -> []
    returnA -< ss { availChords = cs
                  , chord = case cs of
                              [c] -> Just c
                              _ -> Nothing
                  }

  waitForScale :: SF ScaleSelect ScaleSelect
  waitForScale = proc ss -> do
    let scs = let chordScales = nub [ s | c <- availChords ss, s <- map fst $ scalesForChord c ]
                  notes = map toNote $ inputNotes ss
              in  filter (\s -> notes `containedIn` (scaleNotes s)) chordScales
    returnA -< ss { availScales = scs
                  , scale = case scs of
                              [sc] -> Just sc
                              _ -> Nothing
                  }

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