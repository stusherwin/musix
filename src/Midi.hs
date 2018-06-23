module Midi (
  MidiEvent(..),
  MidiConnectionEvent(..),
  MidiAction(..),
  MidiSourceInfo(..),
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

data MidiConnectionEvent = MidiConnectionsChanged [MidiSourceInfo]
                         | MidiConnected Int deriving (Show)

data MidiConnection = MidiConnection { conn :: Connection
                                     , source :: MidiSource
                                     , justConnected :: Bool
                                     }

data MidiSourceInfo = MidiSourceInfo { index :: Int
                                     , name :: String
                                     , model :: String
                                     , manufacturer :: String
                                     , connected :: Bool
                                     } deriving (Eq, Show)

data MidiSource = MidiSource { srcIndex :: Int
                             , srcName :: String
                             , srcModel :: String
                             , srcManufacturer :: String
                             , srcSource :: Source
                             }

data Midi = Midi { sources :: [MidiSource]
                 , connection :: IORef (Maybe MidiConnection)
                 , handler :: IORef (Maybe (M.MidiEvent -> IO ()))
                 }

setupMidi :: IO Midi
setupMidi = do
  connection <- newIORef (Nothing :: Maybe MidiConnection)
  handler <- newIORef (Nothing :: Maybe (M.MidiEvent -> IO ()))
  return Midi { sources = []
              , connection = connection
              , handler = handler
              }

startListening :: Midi -> (MidiConnectionEvent -> IO()) -> (MidiEvent -> IO ()) -> IO ()
startListening midi connHandler eventHandler = do
  writeIORef (handler midi) (Just $ makeHandler eventHandler)
  srcCountRef <- newIORef (0 :: Int)
  let connRef = connection midi
  _ <- forkIO $ loop srcCountRef connRef
  return ()
  where
  makeHandler :: (MidiEvent -> IO ()) -> (M.MidiEvent -> IO ())
  makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOn n _))) = handler (NoteOn n)
  makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOff n _))) = handler (NoteOff n)
  makeHandler _ _ = return ()
  
  loop :: IORef Int -> IORef (Maybe MidiConnection) -> IO ()
  loop srcCountRef connRef = do
    threadDelay 1000
    maybeC <- readIORef connRef
    case maybeC of
      Just c@MidiConnection { justConnected = True } -> do
        writeIORef connRef $ Just c { justConnected = False }
        connHandler $ MidiConnected (srcIndex . source $ c)
        loop srcCountRef connRef
      _ -> do
        srcCount <- readIORef srcCountRef
        srcs <- getSources
        when (srcCount /= length srcs) $ do
          writeIORef srcCountRef (length srcs)
          connHandler $ MidiConnectionsChanged $ map toSrcInfo srcs
        loop srcCountRef connRef
  
  toSrcInfo :: MidiSource -> MidiSourceInfo
  toSrcInfo src = MidiSourceInfo (srcIndex src) (srcName src) (srcModel src) (srcManufacturer src) False

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
  let maybeSrc = find ((== i) . srcIndex) (sources midi)
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