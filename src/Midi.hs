module Midi (
  Midi,
  MidiEvent(..),
  setupMidi,
  cleanUpMidi
) where

import Control.Monad ( forM_ )
import System.MIDI ( Connection, MidiMessage(..), enumerateSources, openSource, getName, start, stop, close )
import qualified System.MIDI as M ( MidiEvent(..), MidiMessage'(..) )

data MidiEvent = NoteOn Int
               | NoteOff Int deriving (Show)

newtype Midi = Midi { getConn :: Maybe Connection } 

makeHandler :: (MidiEvent -> IO ()) -> (M.MidiEvent -> IO ())
makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOn n _))) = handler (NoteOn n)
makeHandler handler (M.MidiEvent _ (MidiMessage _ (M.NoteOff n _))) = handler (NoteOff n)
makeHandler _ _ = return ()

setupMidi :: (MidiEvent -> IO ()) -> IO Midi
setupMidi handler = do
  sources <- enumerateSources
  forM_ sources (\s -> do
    n <- getName s
    putStrLn n)
  if length sources > 0 then do
    let source = sources !! 0
    c <- openSource source $ Just $ makeHandler handler
    start c
    return Midi { getConn = Just c }
  else
    return Midi { getConn = Nothing }

cleanUpMidi :: Midi -> IO ()
cleanUpMidi midi = case getConn midi of
                     (Just c) -> do
                       stop c
                       close c
                     _ -> return ()