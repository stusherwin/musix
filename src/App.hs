{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}

module App where

import Control.Arrow ( returnA )
import FRP.Yampa ( Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>), mapFilterE, maybeToEvent, mergeBy, iterFrom, hold, sscan, sscanPrim, edge, DTime(..) )
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
    scaleSelect' <- changeScale -< (keys', scaleSelect state)
  
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
    virtualMidi' e@(Event (UI (UIKeyDown c))) = maybe e (Event . Midi . NoteOn) $ findNote c
    virtualMidi' e@(Event (UI (UIKeyUp c)))   = maybe e (Event . Midi . NoteOff) $ findNote c
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

changeScale :: SF (Event [Int], ScaleSelect) ScaleSelect
changeScale = identity &&& detectParsingState >>> rSwitch (arr snd) where
  detectParsingState :: SF (Event [Int], ScaleSelect) (Event (SF (Event [Int], ScaleSelect) ScaleSelect))
  detectParsingState = proc (keys, ss) -> do
    parseTrigger <- lastNBlocksPlayedSameWithin 3 0.5 -< keys

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

lastNBlocksPlayedSameWithin :: Int -> DTime -> SF (Event [Int]) (Event ())
lastNBlocksPlayedSameWithin n t = findLastPlayedBlock >>> compareLastNBlocksWithin n t >>> edge
  where
  findLastPlayedBlock :: SF (Event [Int]) (Event [Int])
  findLastPlayedBlock = sscanPrim fn ([], [], True) NoEvent where
    fn :: ([Int], [Int], Bool) -> Event [Int] -> Maybe (([Int], [Int], Bool), Event [Int])
    fn (lastBlock, ns, True) (Event ns') | length ns' > length ns = Just ((lastBlock, ns', True), NoEvent)
    fn (_, ns, True) (Event ns') | length ns' < length ns = Just ((ns, ns', False), Event ns)
    fn (lastBlock, _, False) (Event []) = Just ((lastBlock, [], True), NoEvent)
    fn (lastBlock, ns, b) _ = Just ((lastBlock, ns, b), NoEvent)

  compareLastNBlocks :: Int -> SF (Event [Int]) Bool
  compareLastNBlocks n = sscanPrim fn (replicate (n - 1) []) False where
    fn :: [[Int]] -> Event [Int] -> Maybe ([[Int]], Bool)
    fn bs (Event b2) | all (== b2) bs = Just ((replicate (n - 1) []), True)
    fn bs (Event b2) = Just (take (n - 1) (b2:bs), False)
    fn bs _ = Just (bs, False)

  compareLastNBlocksWithin :: Int -> DTime -> SF (Event [Int]) Bool
  compareLastNBlocksWithin n totalTime = iterFrom fn ([], False) >>> arr snd where
    fn :: Event [Int] -> Event [Int] -> DTime -> ([([Int], DTime)], Bool) -> ([([Int], DTime)], Bool)
    fn _ (Event bs) t (xs, _) = let xs' = take (n - 1) xs
                                in if length xs' == n - 1
                                      && all ((== bs) . fst) xs'
                                      && t + sum (map snd xs') < totalTime
                                   then ([], True)
                                   else ((bs, t):xs', False)
    fn _ _ t1 (((bs, t0):xs), _) = ((bs, t0+t1):xs, False)
    fn _ _ _ (xs, _) = (xs, False)