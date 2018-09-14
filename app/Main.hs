import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import FRP.Yampa ( Event(..), SF, reactInit, react, loopPre, arr, afterEach, time, (>>>), (<<<), reactimate )
import Control.Concurrent ( threadDelay, forkIO, yield )
import Control.Monad ( when )
import Debug.Trace ( trace )
import Data.Time.Clock ( UTCTime, getCurrentTime, diffUTCTime ) 

import Config
import Music
import Midi
import UI
import App
import AppState

twoSecondsPassed :: SF () Bool
twoSecondsPassed = time >>> arr (> 2) --(\t -> (trace (show t) t > 2.0))
 
main' :: IO ()
main' = do
  t <- getCurrentTime
  timeRef <- newIORef t
  reactimate initialize (sense timeRef) actuate ((afterEach $ cycle [(0.9, "down"), (0.1, "up")]) <<< twoSecondsPassed)
 
initialize :: IO ()
initialize = putStrLn "Hello... wait for it..."
 
actuate :: Bool -> Event String -> IO Bool
actuate _ (Event x) = (putStrLn x) >> return False
actuate _ _ = return False
 
sense :: IORef UTCTime -> Bool -> IO (Double, Maybe ())
sense timeRef _ = do
  now      <- getCurrentTime
  lastTime <- readIORef timeRef
  writeIORef timeRef now --(trace (show now) now)
  let dt = now `diffUTCTime` lastTime
  return (realToFrac dt, Just ())

main :: IO ()
main = do
  config <- getConfig "config.txt"
  let keyboard = makeKeyboard 0 83 --case source of 
                  --  Just (s, (fk, lk)) -> makeKeyboard (name s) fk lk
                  --  _ -> makeKeyboard "Virtual Keyboard" 0 83
  midi <- setupMidi
  (handleUI, handleMidiEvent, handleMidiConnection) <- setupYampa exitUI $ loopPre (initState keyboard) (mainSF midi)
  startListening midi handleMidiConnection handleMidiEvent
  setupUI handleUI
  startUI
  closeMidi midi
  
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
           \e -> react' $ Event (MidiConnection e) )