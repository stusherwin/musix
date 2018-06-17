{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}

module App where

import Control.Arrow ( returnA )
import FRP.Yampa ( Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>), mapFilterE, maybeToEvent )
import Data.List ( elemIndex, intercalate, find, delete, sort, (\\), nub )
import Data.Map.Lazy ( (!) )
import Data.Maybe ( listToMaybe )

import AppState
import Midi
import Music
import UI

data EventType = UI UIEvent
               | Midi MidiEvent deriving (Show) 

getUIEvent :: EventType -> Maybe UIEvent
getUIEvent (UI e) = Just e
getUIEvent _ = Nothing

getMidiEvent :: EventType -> Maybe MidiEvent
getMidiEvent (Midi m) = Just m
getMidiEvent _ = Nothing

mainSF :: SF (Event EventType, State) (Event (IO Bool), State)
mainSF = proc (event, state) -> do
  let uiE = (mapFilterE getUIEvent) event
  vmidi <- virtualMidi -< uiE
  uiA <- uiAction -< uiE
  let midiE = rMerge vmidi (mapFilterE getMidiEvent $ event)
  keyboard' <- pressKey -< (midiE, keyboard state)
  scaleSelect' <- changeScale -< (keyboard', scaleSelect state, midiE)

  let state' = state { keyboard = keyboard'
                     , scaleSelect = scaleSelect'
                     }

  let io = handleUIAction state' <$> uiA

  returnA -< (io, state')

virtualMidi :: SF (Event UIEvent) (Event MidiEvent)
virtualMidi = proc e -> do
  returnA -< case e of
    Event (UIKeyDown c) -> maybeToEvent $ NoteOn <$> findNote c
    Event (UIKeyUp c)   -> maybeToEvent $ NoteOff <$> findNote c
    _ -> NoEvent
  where
    keys = ['z', 's', 'x', 'd', 'c', 'v', 'g', 'b', 'h', 'n', 'j', 'm',
            'q', '2', 'w', '3', 'e', 'r', '5', 't', '6', 'y', '7', 'u']
    findNote c = c `elemIndex` keys

uiAction :: SF (Event UIEvent) (Event UIAction)
uiAction = proc e -> do
  returnA -< case e of
    Event (UIKeyDown '\27') -> Event $ UIExit
    Event UIRequestRefresh -> Event $ UIRefresh
    Event (UIRequestReshape size) -> Event $ UIReshape size
    _ -> NoEvent

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