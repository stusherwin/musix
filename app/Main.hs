import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import FRP.Yampa ( Event(..), SF, reactInit, react, loopPre )
import Control.Concurrent ( threadDelay )
import Debug.Trace (trace)

import Music
import Midi
import UI
import App
import AppState

initialState = initState (makeKeyboard 0 83) --21 108) -- 0 83

main :: IO ()
main = do
  (handleUI, handleMidi) <- setupYampa exitUI $ loopPre initialState mainSF
  midi <- setupMidi handleMidi
  setupUI handleUI
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