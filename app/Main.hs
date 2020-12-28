{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.Map.Strict as M
import qualified Sound.MIDI.File as MIDIFile
import qualified Sound.MIDI.File.Event as Event
import qualified Sound.MIDI.Message.Channel as Message
import qualified Sound.MIDI.Message.Channel.Voice as Voice
-- import qualified Sound.MIDI.File.Event.Meta as MetaEvent
import Data.EventList.Relative.TimeBody (toAbsoluteEventList)
import Data.Function ((&))
import Data.Ratio ((%))
import Data.Maybe (fromMaybe)
import Diagrams.Backend.SVG (renderSVG)
import Diagrams.TwoD.Size
import Options.Applicative
import Text.Read (readMaybe)

import Sound.MIDI.File.Load (fromFile)
import System.FilePath ((-<.>))

import EqualTemperament
    ( adjustPitchBySemitones
    , shiftToBelowNote
    , centreNoteOfKey
    , semitonesBetweenKeys'

    , Key(..)
    , keyFromName
    , Pitch(..)
    , pitchFromMidiPitch
    , pitchToMidiValue)
import PianoRoll
import Types
import Utils

data NoteEvent = On | Off deriving (Eq, Show)
type NoteEvents t = M.Map Voice.Pitch [(t, NoteEvent)]

separatePitches :: t -> Event.T -> NoteEvents t -> NoteEvents t
separatePitches time (Event.MIDIEvent (Message.Cons _ (Message.Voice voice)))
      = handleNote voice
    where handleNote (Voice.NoteOn pitch _)  = M.insertWith (++) pitch [(time, On)]
          handleNote (Voice.NoteOff pitch _) = M.insertWith (++) pitch [(time, Off)]
          handleNote _ = id
separatePitches _ _ = id

trackToNoteEvents :: AbsEventList.T t Event.T -> NoteEvents t
trackToNoteEvents = AbsEventList.foldrPair separatePitches M.empty

joinNoteEvents :: [(t, NoteEvent)] -> [(t, t)]
joinNoteEvents []  = []
joinNoteEvents [_] = []
joinNoteEvents ((_, Off):ns) = joinNoteEvents ns
joinNoteEvents (n0@(_, On):(_, On):ns) = joinNoteEvents (n0:ns)
joinNoteEvents ((t0, On):(t1, Off):ns) = (t0, t1) : joinNoteEvents ns

eventsToDurations :: NoteEvents t -> RangeData Voice.Pitch t
eventsToDurations = M.map joinNoteEvents

normaliseToMiddleC :: Key -> RangeData Pitch a -> RangeData Pitch a
normaliseToMiddleC = normaliseToMiddleOfKey (read "C")

normaliseToMiddleOfKey :: Key -> Key -> RangeData Pitch a -> RangeData Pitch a
normaliseToMiddleOfKey targetKey originalKey notes =
    mapRangeKeys (adjustPitchBySemitones totalShift) notes

    where keyShift = semitonesBetweenKeys' targetKey originalKey
          bottomNote = fst $ M.findMin notes
          noteShift = shiftToBelowNote (centreNoteOfKey targetKey)
                                       bottomNote
          totalShift = keyShift + noteShift

runProgram :: ProgramOptions -> IO ()
runProgram ProgramOptions{..} = do
    -- Load MIDI file
    (MIDIFile.Cons _ division tracks) <-
        MIDIFile.explicitNoteOff <$> fromFile inputFile

    let ppq = MIDIFile.ticksPerQuarterNote division

        -- We only care about the first track
        track = head tracks
                   -- Convert times to absolute
                   & toAbsoluteEventList mempty
                   -- Convert times to quarter notes
                   & AbsEventList.mapTime ((% ppq) . fromIntegral)

        allNotes = mapRangeKeys pitchFromMidiPitch
            . eventsToDurations $ trackToNoteEvents track

        outputFile' = fromMaybe (inputFile -<.> ".svg") outputFile

        lastBar = maximum $ fmap (maximum . map snd) allNotes

        barRange = (fromIntegral startBar, maybe lastBar fromIntegral endBar)

        clippedNotes = mapRanges ignoreClip $ clipDataXRange allNotes barRange
        clippedNotesInKey = normaliseToMiddleOfKey targetKey inputKey clippedNotes

        barAxis   = AxisRange $ bimapBoth ratioToFrac barRange
        pitchAxis = AxisRange $ bimapBoth pitchToMidiValue (minimumPitch, maximumPitch)

        rollData = clippedNotesInKey & mapRanges ratioToFrac
                                     & mapRangeKeys fromPitch
        diagram = pianoRoll rollData beatsPerBar barAxis pitchAxis

    renderSVG outputFile' (mkWidth (fromIntegral outputWidth)) diagram

data ProgramOptions = ProgramOptions
  { inputFile  :: Path.FilePath
  , inputKey :: Key
  , outputFile :: Maybe Path.FilePath
  , targetKey :: Key
  , startBar :: Int
  , endBar :: Maybe Int
  , beatsPerBar :: Int
  , beatUnit :: Int
  , minimumPitch :: Pitch
  , maximumPitch :: Pitch
  , outputWidth :: Int
  }

optionParser :: Parser ProgramOptions
optionParser = ProgramOptions
    <$> strArgument (
           help "input midi file"
        <> metavar "INPUT-FILE" )
    <*> argument (maybeReader keyFromName) (
           help "input key of piece"
        <> metavar "INPUT-KEY" )
    <*> option readMaybeMaybe (
           long "output-file"
        <> short 'o'
        <> value Nothing
        <> showDefaultWith (const "<INPUT-FILE>.svg")
        <> help "name of output SVG file" )
    <*> option auto (
           long "target-key"
        <> value (read "C")
        <> showDefault
        <> help "key to transpose" )
    <*> option auto (
           long "start-bar"
        <> value 0
        <> showDefault
        <> help "starting bar from which to plot" )
    <*> option readMaybeMaybe (
           long "end-bar"
        <> value Nothing
        <> showDefaultWith (const "last bar in song")
        <> help "last bar to plot" )
    <*> option auto (
           long "beats-per-bar"
        <> value 4
        <> showDefault
        <> help "e.g. in 3:4, the BPB is 3" )
    <*> option auto (
           long "beat-unit"
        <> value 4
        <> showDefault
        <> help "e.g. in 3:4, the beat unit is 4" )
    <*> option auto (
           long "min-pitch"
        <> value (read "C2")
        <> showDefault
        <> help "lowest allowable pitch to plot" )
    <*> option auto (
           long "max-pitch"
        <> value (read "C5")
        <> showDefault
        <> help "highest allowable pitch to plot" )
    <*> option auto (
           long "output-width"
        <> short 'w'
        <> value 1000
        <> showDefault
        <> help "width of one bar in pixels" )
    where readMaybeMaybe :: (Read a) => ReadM (Maybe a)
          readMaybeMaybe = maybeReader (fmap Just . readMaybe)

programOptions :: ParserInfo ProgramOptions
programOptions = info (optionParser <**> helper) (
    progDesc "Modify the key of a MIDI file and plot the result")

main :: IO ()
main = runProgram =<< execParser programOptions
