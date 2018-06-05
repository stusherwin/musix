{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}
import Control.Arrow ( returnA )
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import Data.List ( elemIndex )
import Data.Map.Lazy ((!))
import qualified Data.Map.Lazy as M ()
import FRP.Yampa ( ReactHandle, Event(..), SF, rMerge, reactInit, react, loopPre )
import Graphics.UI.GLUT  ( getArgsAndInitialize, createWindow, fullScreen, flush, mainLoop, leaveMainLoop )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )
import Control.Concurrent ( threadDelay )

import Graphics
import Keyboard
import Music
import Midi
import ChordMap
import UI

type ReactState = (ReactHandle (Event UIAction, Event MidiEvent) (Event (IO Bool)), IORef POSIXTime)

data EventType = UI UIAction
               | Midi MidiEvent deriving (Show)

data State = State { keyboard :: Keyboard
                   , scale :: Maybe Scale 
                   }

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Musix"
  fullScreen
 
  let state = State { keyboard = makeKeyboard 0 83
                    , scale = Nothing --Just $ Scale Ab Major
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
  scale' <- changeScale -< (midi, scale state)

  let state' = state { keyboard = keyboard'
                     , scale = scale'
                     }

  let io = case (uiAction, keyboard' /= keyboard state) of
            (Event (UIReshape size), _) -> Event (reshape size >> render state >> return False)
            (Event (UIKeyDown '\27'), _) -> Event (render state >> return True)
            (Event UIRefresh, _) -> Event (render state >> return False)
            (_, True) -> Event (render state >> return False)
            _ -> NoEvent

  returnA -< (io, state')

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
  
changeScale :: SF (Event MidiEvent, Maybe Scale) (Maybe Scale)
changeScale = proc (event, maybeScale) -> do
  returnA -< case (event, maybeScale) of
               (Event (NoteOn 0), Nothing) -> Just $ Scale C Major
               (Event (NoteOn 0), Just (Scale n st)) -> Just $ Scale (nextIn notes n) st
               (Event (NoteOn 1), Just (Scale n st)) -> Just $ Scale n (nextIn scaleTypes st)
               (_, scale) -> scale

render :: State -> IO ()
render state = do
  clearScreen
  drawKeyboard (keyboard state) (scale state) (V2 10 50) (V2 1900 180)
  drawText (makeGColor 1 1 1) (V2 100 400) $ maybe "No scale selected" show (scale state)
  --draw (chordMap keyboard) (V2 100 400) (V2 0 0)
  flush