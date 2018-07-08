module Midi (
  MidiEvent(..),
  MidiConnectionEvent(..),
  MidiAction(..),
  MidiSource(..),
  Midi,
  setupMidi,
  startListening,
  handleMidiAction,
  closeMidi
) where

import Control.Concurrent ( threadDelay, forkIO, yield )
import Control.Monad ( forM, when )
import Data.List ( find )
import Data.IORef ( IORef, newIORef, writeIORef, readIORef )
import System.MIDI ( Connection, MidiMessage(..), Source, enumerateSources, openSource, getName, getModel, getManufacturer, start, stop, close )
import qualified System.MIDI as M ( MidiEvent(..), MidiMessage'(..) )
import Debug.Trace ( trace )

data MidiEvent = NoteOn Int
               | NoteOff Int deriving (Show)

data MidiAction = Connect Int
                | Disconnect Int deriving (Show)

data MidiConnectionEvent = MidiConnectionsChanged [MidiSource]
                         | MidiConnected Int deriving (Show)

data MidiConnection = MidiConnection { conn :: Connection
                                     , source :: MidiSource
                                     , justConnected :: Bool
                                     }

data MidiSource = MidiSource { srcIndex :: Int
                             , srcName :: String
                             , srcModel :: String
                             , srcManufacturer :: String
                             , srcSource :: Source
                             } deriving (Eq, Show)

data Midi = Midi { sources :: IORef [MidiSource]
                 , connection :: IORef (Maybe MidiConnection)
                 , handler :: IORef (Maybe (M.MidiEvent -> IO ()))
                 }

setupMidi :: IO Midi
setupMidi = do
  sources <- newIORef []
  connection <- newIORef (Nothing :: Maybe MidiConnection)
  handler <- newIORef (Nothing :: Maybe (M.MidiEvent -> IO ()))
  return Midi { sources = sources
              , connection = connection
              , handler = handler
              }

startListening :: Midi -> (MidiConnectionEvent -> IO()) -> (MidiEvent -> IO ()) -> IO ()
startListening midi connHandler eventHandler = do
  writeIORef (handler midi) (Just $ makeHandler eventHandler)
  _ <- forkIO $ loop
  return ()
  where
  makeHandler :: (MidiEvent -> IO ()) -> (M.MidiEvent -> IO ())
  makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOn n _))) = handler (NoteOn n)
  makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOff n _))) = handler (NoteOff n)
  makeHandler _ _ = return ()
  
  loop :: IO ()
  loop = do
    threadDelay 1000
    srcs <- readIORef $ sources midi
    srcs' <- getSources
    when (length srcs /= length srcs') $ do
      writeIORef (sources midi) srcs'
      connHandler $ MidiConnectionsChanged srcs'
    maybeC <- readIORef $ connection midi
    case maybeC of
      Just c@MidiConnection { justConnected = True } -> do
        writeIORef (connection midi) $ Just c { justConnected = False }
        connHandler $ MidiConnected (srcIndex . source $ c)
      _ -> return ()
    loop
  
closeMidi :: Midi -> IO ()
closeMidi = closeExistingConnection

getSources :: IO [MidiSource]
getSources = do
  sources <- enumerateSources
  forM (zip [0..] sources) $ \(i, s) -> do
    n <- getName s
    m <- getModel s
    mf <- getManufacturer s
    return $ MidiSource i n m mf s

handleMidiAction :: Midi -> MidiAction -> IO Bool
handleMidiAction midi (Connect i) = do
  srcs <- readIORef $ sources midi
  let maybeSrc = find ((== i) . srcIndex) srcs
  case maybeSrc of
    Just src -> do
      closeExistingConnection midi
      maybeH <- readIORef (handler midi)
      case maybeH of
        Just h -> do
          c' <- connect src h
          writeIORef (connection midi) (Just c')
          return False
        _ -> return False
    _ -> return False
handleMidiAction _ _ = return False

closeExistingConnection :: Midi -> IO ()
closeExistingConnection midi = do
  let connRef = connection midi
  maybeC <- readIORef connRef
  case maybeC of
    Just c -> do
      disconnect c
      writeIORef connRef Nothing
    _ -> return ()
    
connect :: MidiSource -> (M.MidiEvent -> IO ()) -> IO MidiConnection
connect src handler = do
  c <- openSource (srcSource src) $ Just handler
  start c
  return $ MidiConnection c src True

disconnect :: MidiConnection -> IO ()
disconnect midi = do
  stop (conn midi)
  close (conn midi)