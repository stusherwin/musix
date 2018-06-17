module Midi (
  MidiConnection,
  MidiEvent(..),
  MidiConnectionEvent(..),
  MidiAction(..),
  MidiSource(index, name, model, manufacturer),
  MidiState(sources),
  setupMidi,
  setupMidiEventHandler,
  updateMidiSources,
  --connect,
  --disconnect,
  handleMidiAction,
  listenForMidiConnections,
  closeExistingConnection
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

data MidiConnectionEvent = MidiConnectionsChanged [MidiSource] deriving (Show)

newtype MidiConnection = MidiConnection { getConn :: Maybe Connection } 

data MidiSource = MidiSource { index :: Int
                             , name :: String
                             , model :: String
                             , manufacturer :: String
                             , source :: Source
                             } deriving (Show)

data MidiState = MidiState { sources :: [MidiSource]
                           , connection :: IORef (Maybe MidiConnection)
                           , midiEventHandler :: IORef (Maybe (MidiEvent -> IO ()))
                           }

makeHandler :: (MidiEvent -> IO ()) -> (M.MidiEvent -> IO ())
makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOn n _))) = handler (NoteOn n)
makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOff n _))) = handler (NoteOff n)
makeHandler _ _ = return ()

setupMidi :: IO MidiState
setupMidi = do
  conn <- newIORef (Nothing :: Maybe MidiConnection)
  handler <- newIORef (Nothing :: Maybe (MidiEvent -> IO ()))
  return MidiState { sources = []
                   , connection = conn
                   , midiEventHandler = handler
                   }

setupMidiEventHandler :: MidiState -> (MidiEvent -> IO ()) -> IO ()
setupMidiEventHandler state handler = do
  writeIORef (midiEventHandler state) (Just handler)

updateMidiSources :: MidiState -> [MidiSource] -> MidiState
updateMidiSources s srcs = s { sources = srcs }

listenForMidiConnections :: (MidiConnectionEvent -> IO()) -> IO ()
listenForMidiConnections handler = do
  srcCountRef <- newIORef (0 :: Int)
  _ <- trace "Listening..." $ forkIO $ loop srcCountRef
  return ()
  where
  loop srcCountRef = do
    threadDelay 1000000
    srcCount <- readIORef srcCountRef
    srcs <- getSources
    when (srcCount /= length srcs) $ do
      writeIORef srcCountRef (length srcs)
      handler $ MidiConnectionsChanged srcs
    loop srcCountRef

getSources :: IO [MidiSource]
getSources = do
  sources <- enumerateSources
  forM (zip [0..] sources) $ \(i, s) -> do
    n <- getName s
    m <- getModel s
    mf <- getManufacturer s
    return $ MidiSource i n m mf s

handleMidiAction :: MidiState -> MidiAction -> IO Bool
handleMidiAction state (Connect i) = do
  let maybeSrc = trace ((show i) ++ (show $ sources state)) $ find ((== i) . index) (sources state)
  case maybeSrc of
    Just src -> trace "got Src" $ do
      closeExistingConnection state
      maybeH <- readIORef (midiEventHandler state)
      case maybeH of
        Just h -> trace "got h" $ do
          c' <- trace "connecting..." $ connect src h
          trace "done." $ writeIORef (connection state) (Just c')
          return False
        _ -> return $ trace "no h" $ False
    _ -> return $ trace "no Src" $ False
handleMidiAction _ _ = return False

closeExistingConnection :: MidiState -> IO ()
closeExistingConnection state = do
  maybeC <- readIORef (connection state)
  case maybeC of
    Just c -> trace "disconnecting..." $ disconnect c
    _ -> trace "no c" $ return ()
  
  
connect :: MidiSource -> (MidiEvent -> IO ()) -> IO MidiConnection
connect src handler = do
    c <- openSource (source src) $ Just $ makeHandler handler
    start c
    return MidiConnection { getConn = Just c }

disconnect :: MidiConnection -> IO ()
disconnect midi = case getConn midi of
                     (Just c) -> do
                       stop c
                       close c
                     _ -> return ()