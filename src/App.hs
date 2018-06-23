{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}

module App where

import Control.Arrow ( returnA )
import FRP.Yampa ( Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>), mapFilterE, maybeToEvent, mergeBy, iterFrom, hold )
import Data.List ( elemIndex, intercalate, find, delete, sort, (\\), nub, deleteBy )
import Data.Map.Lazy ( (!) )
import Data.Maybe ( listToMaybe )

import AppState
import Midi
import Music
import UI

data EventType = UI UIEvent
               | Midi MidiEvent 
               | MidiConnection MidiConnectionEvent deriving (Show) 

getUIEvent :: EventType -> Maybe UIEvent
getUIEvent (UI e) = Just e
getUIEvent _ = Nothing

getMidiEvent :: EventType -> Maybe MidiEvent
getMidiEvent (Midi m) = Just m
getMidiEvent _ = Nothing

getMidiConnectionEvent :: EventType -> Maybe MidiConnectionEvent
getMidiConnectionEvent (MidiConnection m) = Just m
getMidiConnectionEvent _ = Nothing

mainSF :: Midi -> SF (Event EventType, State) (Event (IO Bool), State)
mainSF midi = proc input -> do
  output <- first virtualMidi >>> loopPre NoEvent (mainSF' midi) -< input
  returnA -< output
  where 

  mainSF' :: Midi -> SF ((Event EventType, State), Event [Int]) ((Event (IO Bool), State), Event [Int])
  mainSF' midi = proc ((event, state), keys) -> do
    let uiE = mapFilterE getUIEvent event
    let midiConnE = mapFilterE getMidiConnectionEvent event
    let midiE = mapFilterE getMidiEvent event

    keys' <- (first $ hold []) >>> keysPlayingSF -< (keys, midiE) 
    
    midiA <- midiAction -< midiConnE
    uiA <- uiAction -< uiE

    midiSources' <- updateSources -< (midiConnE, midiSources state)
    keyboard' <- pressKey -< (keys', keyboard state)
    scaleSelect' <- changeScale -< (keyboard', scaleSelect state, midiE)
  
    let state' = state { keyboard = keyboard'
                       , scaleSelect = scaleSelect'
                       , midiSources = midiSources'
                       }
  
    let uiIO = handleUIAction state' <$> uiA
    let midiIO = handleMidiAction midi <$> midiA
  
    returnA -< ((mergeBy (>>) midiIO uiIO, state'), keys')

keysPlayingSF :: SF ([Int], Event MidiEvent) (Event [Int])
keysPlayingSF = proc (keys, midi) -> do
  returnA -< case midi of
               Event (NoteOn k) | not (k `elem` keys) -> Event $ sort $ k : keys
               Event (NoteOff k) -> Event $ delete k keys
               _ -> NoEvent

virtualMidi :: SF (Event EventType) (Event EventType)
virtualMidi = arr virtualMidi'
  where
    virtualMidi' :: Event EventType -> Event EventType
    virtualMidi' e@(Event (UI (UIKeyDown c))) = case findNote c of 
                                                  Just n -> Event $ Midi $ NoteOn n
                                                  _ -> e
    virtualMidi' e@(Event (UI (UIKeyUp c)))   = case findNote c of 
                                                  Just n -> Event $ Midi $ NoteOff n
                                                  _ -> e
    virtualMidi' e = e     
    findNote c = c `elemIndex` keys
    keys = ['z', 's', 'x', 'd', 'c', 'v', 'g', 'b', 'h', 'n', 'j', 'm',
            'q', '2', 'w', '3', 'e', 'r', '5', 't', '6', 'y', '7', 'u']

updateSources :: SF (Event MidiConnectionEvent, [MidiSourceInfo]) [MidiSourceInfo]
updateSources = proc (e, sis) -> do
  returnA -< case e of
    Event (MidiConnectionsChanged srcs) -> srcs
    Event (MidiConnected i) -> let maybeSi = find (\s -> index s == i) sis
                               in  case maybeSi of
                                     Just si -> si { connected = True } : (deleteBy (\a b -> index a == index b) si sis)
                                     _ -> sis
    _ -> sis

uiAction :: SF (Event UIEvent) (Event UIAction)
uiAction = proc e -> do
  returnA -< case e of
    Event (UIKeyDown '\27') -> Event $ UIExit
    Event UIRequestRefresh -> Event $ UIRefresh
    Event (UIRequestReshape size) -> Event $ UIReshape size
    _ -> NoEvent

midiAction :: SF (Event MidiConnectionEvent) (Event MidiAction)
midiAction = proc e -> do
  returnA -< case e of
    Event (MidiConnectionsChanged (s:srcs)) -> Event $ Connect $ Midi.index s
    _ -> NoEvent

pressKey :: SF (Event [Int], Keyboard) Keyboard
pressKey = proc (event, keyboard) -> do
  returnA -< case event of
    Event keys -> keyboard { keysPlaying = keys }
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