import Data.Time.Clock.POSIX ( POSIXTime, getPOSIXTime )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import FRP.Yampa ( Event(..), SF, reactInit, react, loopPre )
import Control.Concurrent ( threadDelay )
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
  
  midiConn <- case source of
    Just (s, _) -> do
      c <- connect s handleMidi
      return $ Just c
    _ -> return Nothing

  setupUI handleUI
  
  case midiConn of
    Just c -> disconnect c
    _ -> return ()

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