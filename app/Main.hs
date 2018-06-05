{-# LANGUAGE Arrows, PackageImports, ScopedTypeVariables #-}
import Control.Arrow ( returnA )
import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import Data.List ( elemIndex, intercalate )
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

data ScaleSelect = ScaleSelect { root :: Maybe Note
                               , scaleType :: Maybe ScaleType
                               , waitingForInput :: Bool
                               , inputNotes :: [Note]
                               , availScaleTypes :: [ScaleType]
                               }
data State = State { keyboard :: Keyboard
                   , scaleSelect :: ScaleSelect
                   }

currentScale (State { scaleSelect = ScaleSelect { root = Just r, scaleType = Just st } }) = Just $ Scale r st
currentScale _ = Nothing

main :: IO ()
main = do
  (_progname, _) <- getArgsAndInitialize
  _window <- createWindow "Musix"
  fullScreen
 
  let state = State { keyboard = makeKeyboard 0 83
                    , scaleSelect = ScaleSelect { root = Nothing
                                                , scaleType = Nothing
                                                , waitingForInput = False
                                                , inputNotes = []
                                                , availScaleTypes = []
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
  scaleSelect' <- changeScale -< (midi, scaleSelect state)

  let state' = state { keyboard = keyboard'
                     , scaleSelect = scaleSelect'
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
  
changeScale :: SF (Event MidiEvent, ScaleSelect) ScaleSelect
changeScale = proc (event, ss) -> do
  returnA -< case (event, ss) of
               (Event (NoteOn 0), ScaleSelect { waitingForInput = False }) ->
                 ss { waitingForInput = True
                    , root = Nothing
                    , scaleType = Nothing
                    , inputNotes = []
                    , availScaleTypes = []
                    }

               (Event (NoteOn 0), ScaleSelect { waitingForInput = True }) ->
                 ss { waitingForInput = False
                    }

               (Event (NoteOn n), ScaleSelect { root = Nothing
                                              , waitingForInput = True
                                              , inputNotes = ns }) ->
                 let r = toNote n
                     ns' = r : ns
                     sts' = scaleTypesFor r $ ns'
                 in  ss { root = Just $ r
                        , inputNotes = ns'
                        , availScaleTypes = sts'
                        }

               (Event (NoteOn n), ScaleSelect { root = Just r
                                              , scaleType = Nothing
                                              , waitingForInput = True
                                              , inputNotes = ns
                                              , availScaleTypes = a:b:sts }) ->
                 let ns' = toNote n : ns
                     sts' = scaleTypesFor r $ ns'
                 in  case sts' of 
                       [] -> ss { inputNotes = ns' 
                                , availScaleTypes = sts'
                                , waitingForInput = False -- remove
                                , root = Nothing          -- remove
                                , scaleType = Nothing     -- remove
                                }
                       [st] -> ss { inputNotes = ns' 
                                  , availScaleTypes = sts'
                                  , waitingForInput = False
                                  , scaleType = Just st
                                  }
                       _ -> ss { inputNotes = ns' 
                               , availScaleTypes = sts'
                               }                 

               (_, _) -> ss

render :: State -> IO ()
render state = do
  clearScreen
  drawKeyboard (keyboard state) (currentScale state) (V2 10 50) (V2 1900 180)
  drawScaleSelect (scaleSelect state)
  --draw (chordMap keyboard) (V2 100 400) (V2 0 0)
  flush

drawScaleSelect :: ScaleSelect -> IO ()
drawScaleSelect (ScaleSelect { root = r, scaleType = st, waitingForInput = w, availScaleTypes = sts }) = do
  drawText (makeGColor 1 1 1) (V2 100 400) (text r st w)
  where
  text (Just r) (Just st) _ = show (Scale r st)
  text Nothing _ True = "Waiting for root..."
  text (Just r) Nothing True = (show r) ++ " " ++ (intercalate "/" $ map show sts)
  text _ _ _ = "No scale selected"