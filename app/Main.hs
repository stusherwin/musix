import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import FRP.Yampa ( Event(..), SF, reactInit, react, loopPre )
import Control.Concurrent ( threadDelay, forkIO, yield )
import Control.Monad ( when )
import Debug.Trace ( trace )

import Config
import Music
import Midi
import UI
import App
import AppState

main :: IO ()
main = do
  config <- getConfig "config.txt"
  let keyboard = makeKeyboard 0 83 --case source of 
                  --  Just (s, (fk, lk)) -> makeKeyboard (name s) fk lk
                  --  _ -> makeKeyboard "Virtual Keyboard" 0 83
  midi <- setupMidi
  (handleUI, handleMidi, handleMidiConnection) <- setupYampa exitUI $ loopPre (initState keyboard midi) mainSF

  setupMidiEventHandler midi handleMidi
  listenForMidiConnections midi handleMidiConnection
  
  setupUI handleUI
  startUI
  closeExistingConnection midi
  
setupYampa :: IO () -> SF (Event EventType) (Event (IO Bool)) -> IO (UIEvent -> IO (), MidiEvent -> IO (), MidiConnectionEvent -> IO ())
setupYampa exit sf = do
  timeRef <- newIORef (0.0 :: POSIXTime)
  let init = return NoEvent
      actuate _ _ NoEvent = return False
      actuate _ _ (Event io) = io
  rh <- reactInit init actuate sf

  let react' e = do
        t' <- getPOSIXTime
        t <- readIORef timeRef
        let dt = realToFrac (t' - t) -- Time difference in seconds
        writeIORef timeRef t'
        shouldExit <- react rh (dt, Just e)
        -- yield
        threadDelay 1000
        if shouldExit
          then exit
        else return ()

  return ( \e -> react' $ Event (UI e),
           \e -> react' $ Event (Midi e),
           \e -> trace (show e ) $ react' $ Event (MidiConnection e) )