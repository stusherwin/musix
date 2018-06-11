{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}
import Control.Arrow ( returnA )
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import Data.List ( elemIndex, intercalate, find, delete )
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as M ()
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

data ScaleSelect = ScaleSelect { root :: Maybe Note
                               , scaleType :: Maybe ScaleType
                               , waitingForInput :: Bool
                               , inputNotes :: [Int]
                               , availScaleTypes :: [ScaleType]
                               , actionKey :: Int
                               } deriving (Eq, Show)

data State = State { keyboard :: Keyboard
                   , scaleSelect :: ScaleSelect
                   } deriving (Eq, Show)

currentScale (State { scaleSelect = ScaleSelect { root = Just r, scaleType = Just st } }) = Just $ Scale r st
currentScale _ = Nothing

clear :: ScaleSelect -> ScaleSelect
clear ss = ss { root = Nothing
              , scaleType = Nothing
              , inputNotes = []
              , availScaleTypes = []
              }

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Musix"
  fullScreen
 
  let keyboard = makeKeyboard 0 83 -- 21 108
      state = State { keyboard = keyboard
                    , scaleSelect = ScaleSelect { root = Nothing
                                                , scaleType = Nothing
                                                , waitingForInput = False
                                                , inputNotes = []
                                                , availScaleTypes = []
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
                   -> Event $ clear ss { waitingForInput = True } -:> waitForRoot
                 (ScaleSelect { waitingForInput = True, actionKey = ak }, Event (NoteOn n)) | n == ak
                   -> Event $ clear ss { waitingForInput = False } -:> arr fst
                 (ScaleSelect { waitingForInput = True, root = Just r, inputNotes = ns}, _) | not (playingNote r ns)
                   -> Event $ ss { root = Nothing } -:> waitForRoot
                 (ScaleSelect { waitingForInput = True, root = Just r, scaleType = Nothing }, _)
                   -> Event $ waitForScaleType r
                 (ScaleSelect { waitingForInput = True, root = Just _, scaleType = Just _ }, _)
                   -> Event $ ss { waitingForInput = False } -:> arr fst
                 _ -> NoEvent
    where
    playingNote note notes = any (\n -> toNote n == note) $ notes
  
  waitForRoot :: SF (ScaleSelect, Event MidiEvent) ScaleSelect
  waitForRoot = proc (ss, midi) -> do
    returnA -< case midi of
                 Event (NoteOn n) -> ss { root = Just $ toNote n, inputNotes = [n] }
                 _ -> ss
  
  waitForScaleType :: Note -> SF (ScaleSelect, Event MidiEvent) ScaleSelect
  waitForScaleType root = proc (ss, midi) -> do
    let ns = inputNotes ss
    let ns' = case midi of
                Event (NoteOn n) -> if n `elem` ns then ns else n : ns
                Event (NoteOff n) -> delete n ns
                _ -> ns
    let sts = scaleTypesFor root $ map toNote ns'
    returnA -< ss { inputNotes = ns'
                  , availScaleTypes = sts
                  , scaleType = case sts of
                                  [st] -> Just st
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
  drawKeyboard (keyboard state) (currentScale state) (V2 10 50) (V2 1900 180)
  drawUIText state
  --draw (chordMap keyboard) (V2 100 400) (V2 0 0)
  flush

drawUIText :: State -> IO ()
drawUIText state = do
  drawText (makeGColor 1 1 1) (V2 100 400) $ "Scale: " ++ (drawScaleSelectText $ scaleSelect state)
  drawText (makeGColor 1 1 1) (V2 100 500) $ "Notes: " ++ (intercalate " " $ map show $ keysPlaying $ keyboard state)
  where
  drawScaleSelectText :: ScaleSelect -> String
  drawScaleSelectText ScaleSelect { root = Just r, scaleType = Just st } = show (Scale r st)
  drawScaleSelectText ScaleSelect { root = Nothing, waitingForInput = True } = "waiting for root..."
  drawScaleSelectText ScaleSelect { root = Just r, scaleType = Nothing, waitingForInput = True, availScaleTypes = [] } = (show r) ++ " ?"
  drawScaleSelectText ScaleSelect { root = Just r, scaleType = Nothing, waitingForInput = True, availScaleTypes = sts } = (show r) ++ " " ++ (intercalate " / " $ map show sts)
  drawScaleSelectText _ = "none"