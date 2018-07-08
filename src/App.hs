{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}

module App where

import Control.Arrow ( returnA )
import FRP.Yampa ( Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>), mapFilterE, maybeToEvent, mergeBy, iterFrom, hold, sscan, sscanPrim, edge, DTime(..), event, edgeBy )
import Data.List ( elemIndex, intercalate, find, delete, sort, (\\), nub, deleteBy )
import Data.Map.Lazy ( (!) )
import Data.Maybe ( listToMaybe )
import Debug.Trace ( trace )

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
  output <- first virtualMidi >>> mainSF' midi -< input
  returnA -< output
  where 

  mainSF' :: Midi -> SF (Event EventType, State) (Event (IO Bool), State)
  mainSF' midi = proc (event, state) -> do
    let uiE = mapFilterE getUIEvent event
    let midiConnE = mapFilterE getMidiConnectionEvent event
    let midiE = mapFilterE getMidiEvent event

    midiA <- midiAction -< midiConnE
    uiA <- uiAction -< uiE

    keyboard' <- updateKeys -< (keyboard state, midiE)
    keysE <- edgeBy (\a b -> if a /= b then Just b else Nothing) [] -< keysPlaying keyboard'
    scaleSelect' <- changeScale -< (keysE, scaleSelect state)   
    midiSources' <- updateSources -< (midiConnE, midiSources state)
  
    let state' = state { keyboard = keyboard'
                       , scaleSelect = scaleSelect'
                       , midiSources = midiSources'
                       }
  
    let uiIO = handleUIAction state' <$> uiA
    let midiIO = handleMidiAction midi <$> midiA
  
    returnA -< (mergeBy (>>) midiIO uiIO, state')

updateKeys :: SF (Keyboard, Event MidiEvent) Keyboard
updateKeys = proc (keyboard, midi) -> do
  returnA -< case midi of
               Event (NoteOn k) -> keyDown k keyboard
               Event (NoteOff k) -> keyUp k keyboard
               _ -> keyboard

virtualMidi :: SF (Event EventType) (Event EventType)
virtualMidi = arr virtualMidi'
  where
    virtualMidi' :: Event EventType -> Event EventType
    virtualMidi' e@(Event (UI (UIKeyDown c))) = maybe e (Event . Midi . NoteOn) $ findNote c
    virtualMidi' e@(Event (UI (UIKeyUp c)))   = maybe e (Event . Midi . NoteOff) $ findNote c
    virtualMidi' e = e
    findNote c = c `elemIndex` keys
    keys = ['z', 's', 'x', 'd', 'c', 'v', 'g', 'b', 'h', 'n', 'j', 'm',
            'q', '2', 'w', '3', 'e', 'r', '5', 't', '6', 'y', '7', 'u']

updateSources :: SF (Event MidiConnectionEvent, [MidiSourceInfo]) [MidiSourceInfo]
updateSources = proc (e, sis) -> do
  returnA -< case e of
    Event (MidiConnectionsChanged srcs) -> map toSrcInfo srcs
    Event (MidiConnected i) -> let maybeSi = find (\s -> index s == i) sis
                               in  case maybeSi of
                                     Just si -> si { connected = True } : (deleteBy (\a b -> index a == index b) si sis)
                                     _ -> sis
    _ -> sis
  where
  toSrcInfo :: MidiSource -> MidiSourceInfo
  toSrcInfo src = MidiSourceInfo (srcIndex src) (srcName src) False

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
    Event (MidiConnectionsChanged (s:srcs)) -> Event $ Connect $ srcIndex s
    _ -> NoEvent

changeScale :: SF (Event [Int], ScaleSelect) ScaleSelect
changeScale = identity &&& detectParsingState >>> rSwitch (arr snd) where
  detectParsingState :: SF (Event [Int], ScaleSelect) (Event (SF (Event [Int], ScaleSelect) ScaleSelect))
  detectParsingState = proc (keys, ss) -> do
    parseTrigger <- detectParseTrigger -< keys

    returnA -< case (parseTrigger, ss, keys) of
      (Event _, ScaleSelect { parsing = False }, Event _)    -> Event $ startParsing ss
      (Event _, ScaleSelect { parsing = True }, _)           -> Event $ cancelParsing ss
      (_, ScaleSelect { parsing = True, scale = Just _ }, _) -> Event $ completeParsing ss
      _ -> NoEvent
  
  startParsing :: ScaleSelect -> SF (Event [Int], ScaleSelect) ScaleSelect
  startParsing ss = clearSS ss { parsing = True } -:> first (hold []) >>> parse

  cancelParsing :: ScaleSelect -> SF (Event [Int], ScaleSelect) ScaleSelect
  cancelParsing ss = clearSS ss { parsing = False } -:> arr snd

  completeParsing :: ScaleSelect -> SF (Event [Int], ScaleSelect) ScaleSelect
  completeParsing ss = ss { parsing = False } -:> arr snd

  detectParseTrigger :: SF (Event [Int]) (Event ())
  detectParseTrigger = lastPlayedBlock
                   >>> timed
                   >>> lastN 3
                   >>> matches (\xs -> length xs == 3
                                    && allSame (map fst xs)
                                    && sum (map snd $ init $ xs) < 1.0)
                   >>> edge

  parse :: SF ([Int], ScaleSelect) ScaleSelect
  parse = proc (keys, ss) -> do
    ssWithRoot <- parseRoot -< (keys, ss)
    ssWithChord <- parseChord -< (keys, ssWithRoot)
    ssWithScale <- parseScale -< (keys, ssWithChord)
    returnA -< ssWithScale
  
  parseRoot :: SF ([Int], ScaleSelect) ScaleSelect
  parseRoot = proc (keys, ss) -> do
    returnA -< case listToMaybe keys of
      Just r -> ss { root = Just $ toNote r }
      _ -> ss { root = Nothing }

  parseChord :: SF ([Int], ScaleSelect) ScaleSelect
  parseChord = proc (keys, ss) -> do
    let cs = case root ss of
               Just r -> chordsForRoot r $ map toNote keys
               _ -> []
    returnA -< ss { availChords = cs
                  , chord = case cs of
                              [c] -> Just c
                              _ -> Nothing
                  }

  parseScale :: SF ([Int], ScaleSelect) ScaleSelect
  parseScale = proc (keys, ss) -> do
    let scs = let chordScales = nub [ s | c <- availChords ss, s <- map fst $ scalesForChord c ]
              in  filter (\s -> (map toNote keys) `containedIn` (scaleNotes s)) chordScales
    returnA -< ss { availScales = scs
                  , scale = case scs of
                              [sc] -> Just sc
                              _ -> Nothing
                  }

lastPlayedBlock :: SF (Event [Int]) (Event [Int])
lastPlayedBlock = sscanPrim fn ([], [], True) NoEvent where
  fn :: ([Int], [Int], Bool) -> Event [Int] -> Maybe (([Int], [Int], Bool), Event [Int])
  fn (lastBlock, ns, True) (Event ns') | length ns' > length ns = Just ((lastBlock, ns', True), NoEvent)
  fn (_, ns, True) (Event ns') | length ns' < length ns = Just ((ns, ns', False), Event ns)
  fn (lastBlock, _, False) (Event []) = Just ((lastBlock, [], True), NoEvent)
  fn (lastBlock, ns, b) _ = Just ((lastBlock, ns, b), NoEvent)

allSame :: Eq a => [a] -> Bool
allSame (x:xs) = all (== x) xs
allSame _ = True

timed :: SF (Event a) (Event (a, DTime))
timed = iterFrom fn (0.0, NoEvent) >>> arr snd where
  fn :: Event a -> Event a -> DTime -> (DTime, Event (a, DTime)) -> (DTime, Event (a, DTime))
  fn _ (Event a) t1 (t0, _) = (t1, Event (a, t0))
  fn _ _ t1 (t0, _) = (t0 + t1, NoEvent)

lastN :: Int -> SF (Event a) (Event [a])
lastN n = sscanPrim fn [] NoEvent where
  fn :: [a] -> Event a -> Maybe ([a], Event [a])
  fn as (Event a) = let as' = take n (a:as) in Just (as', Event as')
  fn as _ = Just (as, NoEvent)

matches :: ([a] -> Bool) -> SF (Event [a]) Bool
matches predicate = arr $ event False predicate