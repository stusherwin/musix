module Midi (
  MidiConnection,
  MidiEvent(..),
  MidiSource(index, name, model, manufacturer),
  setupMidi,
  connect,
  disconnect
) where

import Control.Monad ( forM )
import System.MIDI ( Connection, MidiMessage(..), Source, enumerateSources, openSource, getName, getModel, getManufacturer, start, stop, close )
import qualified System.MIDI as M ( MidiEvent(..), MidiMessage'(..) )

data MidiEvent = NoteOn Int
               | NoteOff Int deriving (Show)

newtype MidiConnection = MidiConnection { getConn :: Maybe Connection } 

makeHandler :: (MidiEvent -> IO ()) -> (M.MidiEvent -> IO ())
makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOn n _))) = handler (NoteOn n)
makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOff n _))) = handler (NoteOff n)
makeHandler _ _ = return ()

setupMidi :: IO [MidiSource]
setupMidi = do
  sources <- enumerateSources
  forM (zip [0..] sources) $ \(i, s) -> do
    n <- getName s
    m <- getModel s
    mf <- getManufacturer s
    return $ MidiSource i n m mf s
  
data MidiSource = MidiSource { index :: Int
                             , name :: String
                             , model :: String
                             , manufacturer :: String
                             , source :: Source
                             } deriving (Show)

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