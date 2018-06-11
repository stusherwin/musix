{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}
import Control.Arrow ( returnA )
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import Data.List ( elemIndex, intercalate, find, delete, sort, (\\), nub )
import Data.Maybe ( listToMaybe )
import Data.Map.Lazy ( (!) )
import FRP.Yampa ( ReactHandle, Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch, (-->), (-:>) )
import FRP.Yampa.Delays ( fby )
import Graphics.UI.GLUT  ( getArgsAndInitialize, createWindow, fullScreen, flush, mainLoop, leaveMainLoop )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Concurrent ( threadDelay )
import Debug.Trace (trace)

import Graphics
import Keyboard
import Music
import Midi
import ChordMap
import UI

type ReactState = (ReactHandle (Event UIAction, Event MidiEvent) (Event (IO Bool)), IORef POSIXTime)

data EventType = UI UIAction
               | Midi MidiEvent deriving (Show) 

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

clear :: ScaleSelect -> ScaleSelect
clear ss = ss { scale = Nothing
              , chord = Nothing
              , inputNotes = []
              , root = Nothing
              , availScales = []
              , availChords = []
              }

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Musix"
  fullScreen
 
  let keyboard = makeKeyboard 21 108 -- 0 83
      state = State { keyboard = keyboard
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

  (handleUI, handleMidi) <- setupYampa leaveMainLoop $ loopPre state mainSF

  midi <- setupMidi handleMidi
  setupUI handleUI

  mainLoop

  cleanUpMidi midi

setupYampa :: IO () -> SF (Event EventType) (Event (IO Bool)) -> IO (UIAction -> IO (), MidiEvent -> IO ())
setupYampa exit sf = do
  timeRef <- newIORef (0.0 :: POSIXTime)
  let init = return NoEvent
      actuate _ _ NoEvent = return False
      actuate _ _ (Event io) = io
  rh <- reactInit init actuate sf

  let react' e = do
        threadDelay 1000
        t' <- getPOSIXTime
        t <- readIORef timeRef
        let dt = realToFrac (t' - t) -- Time difference in seconds
        writeIORef timeRef t'
        shouldExit <- react rh (dt, Just e)
        if shouldExit
          then exit
        else return ()

  return ( \uiAction -> react' $ Event (UI uiAction),
           \midiEvent -> react' $ Event (Midi midiEvent ) )

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

  let io = ui state' uiAction

  returnA -< (io, state')

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

ui :: State -> Event UIAction -> Event (IO Bool)
ui state (Event (UIReshape size)) = Event (reshape size >> render state >> return False)
ui state (Event (UIKeyDown '\27')) = Event (render state >> return True)
ui state (Event UIRefresh) = Event (render state >> return False)
ui _ _ = NoEvent

getUIAction :: Event EventType -> Event UIAction
getUIAction (Event (UI uiAction)) = Event uiAction
getUIAction _ = NoEvent

getMidi :: Event EventType -> Event MidiEvent
getMidi (Event (Midi midi)) = Event midi
getMidi _ = NoEvent

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

render :: State -> IO ()
render state = do
  clearScreen
  drawKeyboard (keyboard state) (scale $ scaleSelect state) (V2 10 50) (V2 1900 180)
  drawUIText state
  --draw (chordMap keyboard) (V2 100 400) (V2 0 0)
  flush

drawUIText :: State -> IO ()
drawUIText state = do
  drawText (makeGColor 1 1 1) (V2 100 400) $ "Notes: " ++ (intercalate " " $ map (show . toNote) $ keysPlaying $ keyboard state)
  drawText (makeGColor 1 1 1) (V2 100 500) $ "Chord: " ++ (drawChordText $ scaleSelect state)
  drawText (makeGColor 1 1 1) (V2 100 600) $ "Scale: " ++ (drawScaleText $ scaleSelect state)
  
  where
  
  drawChordText :: ScaleSelect -> String
  drawChordText ScaleSelect { chord = Just sc } = show sc
  drawChordText ScaleSelect { chord = Nothing, waitingForInput = True, inputNotes = [] } = "waiting for chord..."
  drawChordText ScaleSelect { chord = Nothing, waitingForInput = True, availChords = [], root = Just r } = show r ++ " ?"
  drawChordText ScaleSelect { chord = Nothing, waitingForInput = True, availChords = cs } = intercalate " / " $ map show cs
  drawChordText _ = "none"

  drawScaleText :: ScaleSelect -> String
  drawScaleText ScaleSelect { scale = Just sc, root = Just r } = showInKey r sc
  drawScaleText ScaleSelect { scale = Nothing, waitingForInput = True, inputNotes = [] } = "waiting for scale..."
  drawScaleText ScaleSelect { scale = Nothing, waitingForInput = True, availScales = [] } = "?"
  drawScaleText ScaleSelect { scale = Nothing, waitingForInput = True, availScales = scs, root = Just r } = intercalate " / " $ map (showInKey r) scs
  drawScaleText _ = "none"