{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}
import Control.Arrow ( returnA )
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import Data.List ( elemIndex, intercalate, find, delete )
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as M ()
import FRP.Yampa ( ReactHandle, Event(..), SF, rMerge, reactInit, react, loopPre, constant, rSwitch, arr, (&&&), first, second, (>>>), identity, tag, drSwitch, kSwitch, dkSwitch, switch, dSwitch )
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
                               , actionKeyPressed :: Bool
                               } deriving (Eq, Show)

data State = State { keyboard :: Keyboard
                   , scaleSelect :: ScaleSelect
                   } deriving (Eq, Show)

currentScale (State { scaleSelect = ScaleSelect { root = Just r, scaleType = Just st } }) = Just $ Scale r st
currentScale _ = Nothing

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
                                                , actionKeyPressed = False
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

type ScaleSelectSF = SF (ScaleSelect, Event MidiEvent) ScaleSelect

changeScale :: SF (ScaleSelect, Event MidiEvent) ScaleSelect
changeScale = proc (scaleSelect, midi) -> do
  scaleSelect' <- action -< (scaleSelect, midi)
  scaleSelect'' <- decision None -< (scaleSelect', midi)
  returnA -< scaleSelect''

data X = None | WaitForRoot | WaitForScaleType Note deriving (Show)

decision :: X -> SF (ScaleSelect, Event MidiEvent) ScaleSelect
decision x = dSwitch (sf x) decision
  where
  sf :: X -> SF (ScaleSelect, Event MidiEvent) (ScaleSelect, Event X)
  sf None = proc i@(scaleSelect, midi) -> do
    let out = case (waitingForInput scaleSelect, root scaleSelect, scaleType scaleSelect, midi) of
                 (True, Nothing, _, _) -> trace ("none in:" ++ show i) $ (scaleSelect, Event WaitForRoot)
                 (True, Just _, Just _, _) -> trace ("none in):" ++ show i) $ (scaleSelect { waitingForInput = False }, NoEvent)
                 _ -> (scaleSelect, NoEvent)
    returnA -< case midi of
                 Event _ -> trace ("none out: " ++ show out) $ out
                 _ -> out
  sf WaitForRoot = proc i@(scaleSelect, midi) -> do
    let out = case midi of
                Event (NoteOn n) -> trace ("waitForRoot in: " ++ show i) $ (scaleSelect { root = Just $ toNote n, inputNotes = [n] }, Event $ WaitForScaleType (toNote n))
                Event (NoteOff n) -> trace ("waitForRoot in: " ++ show i) $ (scaleSelect, NoEvent)
                _ -> (scaleSelect, NoEvent)
    returnA -< case midi of
                 Event _ -> trace ("waitForRoot out: " ++ show out) $ out
                 _ -> out
  sf (WaitForScaleType root) = proc i@(scaleSelect, midi) -> do
    let ns = inputNotes scaleSelect
    let ns' = case midi of
                Event (NoteOn n) -> trace ("waitForScaleType in: " ++ show i) $ if n `elem` ns then ns else n : ns
                Event (NoteOff n) -> trace ("waitForScaleType in: " ++ show i) $ delete n ns
                _ -> ns
    let availScaleTypes' = scaleTypesFor root $ map toNote ns'
    let playingRoot = any (\n -> toNote n == root) ns'
    let root' = if playingRoot then Just root else Nothing
    let scaleType' = case availScaleTypes' of
                       [st] -> Just st
                       _ -> Nothing
    let out = (scaleSelect { inputNotes = ns'
                           , availScaleTypes = availScaleTypes'
                           , scaleType = scaleType'
                           , root = root'
                           }, case (scaleType', root') of
                                (Just _, _) -> Event None
                                (_, Nothing) -> Event None
                                _ -> NoEvent)
    returnA -< case midi of
                 Event _ -> trace ("waitForScaleType out: " ++ show out) $ out
                 _ -> out

action :: SF (ScaleSelect, Event MidiEvent) ScaleSelect
action = proc i@(scaleSelect, midi) -> do
  let out = case (midi, waitingForInput scaleSelect) of
              (Event (NoteOn n), False) | n == (actionKey scaleSelect) -> trace ("action in: " ++ show i) $ scaleSelect { waitingForInput = True, root = Nothing, scaleType = Nothing, inputNotes = [], availScaleTypes = [] }
              (Event (NoteOn n), True) | n == (actionKey scaleSelect) -> trace ("action in: " ++ show i) $ scaleSelect { waitingForInput = False, root = Nothing, scaleType = Nothing, inputNotes = [], availScaleTypes = [] }
              _ -> scaleSelect
  returnA -< case midi of
               Event _ -> trace ("action out: " ++ show out) $ out
               _ -> out

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
  drawScaleSelectText ScaleSelect { root = Just r, scaleType = Nothing, waitingForInput = True, availScaleTypes = sts } = (show r) ++ " " ++ (intercalate " / " $ map show sts) ++ " ?"
  drawScaleSelectText _ = "none"