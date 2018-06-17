import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import FRP.Yampa ( Event(..), SF, reactInit, react, loopPre )
import Control.Concurrent ( threadDelay, forkIO, yield )
import Control.Monad ( when )
import Debug.Trace ( trace )
import System.Directory ( doesFileExist )
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes )
import Text.Read ( readMaybe )

import Music
import Midi
import UI
import App
import AppState

parseConfig :: String -> [(String, (Int, Int))]
parseConfig = catMaybes . map (parseLine . splitOn "|") . lines
  where
  parseLine (name:firstKey:lastKey:_) =
    case (readMaybe firstKey :: Maybe Int, readMaybe lastKey :: Maybe Int) of
      (Just fk, Just lk) -> Just (name, (fk, lk))
      _ -> Nothing
  parseLine _ = Nothing

main :: IO ()
main = do
  sources <- setupMidi
  
  fileExists <- doesFileExist "config.txt"
  when (not fileExists) $ do
    writeFile "config.txt" "" -- $ unlines $ map name sources

  config <- readFile "config.txt"
  let sourceConfigs = map (\s -> (s, lookup (name s) (parseConfig config))) sources
  let source = case trace ("Midi sources: " ++ (unlines $ map show sourceConfigs)) sourceConfigs of
                 (s, Just c):_ -> Just (s, c)
                 _ -> Nothing

  let keyboard = case source of 
                   Just (s, (fk, lk)) -> makeKeyboard (name s) fk lk
                   _ -> makeKeyboard "Virtual Keyboard" 0 83
     
  (handleUI, handleMidi) <- setupYampa exitUI $ loopPre (initState keyboard) mainSF
  forkIO (timer handleMidi)
  
  midiConn <- case source of
    Just (s, _) -> do
      c <- connect s handleMidi
      return $ Just c
    _ -> return Nothing

  setupUI handleUI
  
  case midiConn of
    Just c -> disconnect c
    _ -> return ()
  where
  timer :: (MidiEvent -> IO()) -> IO ()
  timer midi = do
    midi (NoteOn 0)
    threadDelay 100000
    midi (NoteOff 0)
    threadDelay 100000
    timer midi


setupYampa :: IO () -> SF (Event EventType) (Event (IO Bool)) -> IO (UIEvent -> IO (), MidiEvent -> IO ())
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

  return ( \uiAction -> react' $ Event (UI uiAction),
           \midiEvent -> react' $ Event (Midi midiEvent ) )