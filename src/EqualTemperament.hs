{-# LANGUAGE LambdaCase #-}

module EqualTemperament
  ( isSharp
  , noteNames
  , centreNoteOfKey

  , Pitch(fromPitch)
  , pitchFromName
  , pitchFromName'
  , pitchFromMidiValue
  , pitchFromMidiPitch
  , pitchToMidiValue

  , Key(fromKey)
  , keyFromName

  , semitonesBetweenKeys
  , semitonesBetweenKeys'
  , semitonesBetweenPitches
  , shiftToBelowNote
  , adjustPitchBySemitones
  ) where

import qualified Data.Set as S
import qualified Sound.MIDI.Message.Channel.Voice as Voice
import Control.Monad (liftM2)
import Data.Char (toUpper)
import Data.Functor (($>))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

newtype Pitch = Pitch { fromPitch :: Int } deriving (Eq, Ord)
newtype Key = Key { fromKey :: Int } deriving (Eq, Ord)

{- MIDI pitches start at 21: A0, 22: B0, ..., 108: C7

        C    C#   D    D#   E    F    F#   G    G#   A    A#   B
  -1    0    1    2    3    4    5    6    7    8    9   10   11
   0   12   13   14   15   16   17   18   19   20   21   22   23
   1   24   25   26   27   28   29   30   31   32   33   34   35
   2   36   37   38   39   40   41   42   43   44   45   46   47
  ...
-}

instance Read Pitch where
    readsPrec = const $ readP_to_S readPitch
instance Show Pitch where
    show = pitchToSPN'

instance Read Key where
    readsPrec = const $ readP_to_S readKey
instance Show Key where
    show (Key k) = fancify $ noteNames !! k

readAccidental :: ReadP Int
readAccidental = (satisfy (`elem` "#♯") $> 1)
             +++ (satisfy (`elem` "b♭") $> (-1))

readAccidental' = readAccidental <++ return 0

readKey :: ReadP Key
readKey = Key . (`mod` 12) <$> liftM2 (+) readBasePitch readAccidental'

readBasePitch :: ReadP Int
readBasePitch = do
    note <- toUpper <$> get
    return . fromJust $ elemIndex [note] noteNames

readOctave :: ReadP Int
readOctave = do
    o <- get
    return $ (1 + read [o]) * 12

readPitch :: ReadP Pitch
readPitch = pitchFromMidiValue . sum <$>
                sequence [readBasePitch, readAccidental', readOctave]

noteNames = ["C", "C#/Db", "D", "D#/Eb", "E", "F", "F#/Gb", "G", "G#/Ab", "A", "A#/Bb", "B"]
-- midiNoteNames = M.fromList $
--     zip [0..127] [note ++ show octave | octave <- [-1..],
--                                         note <- noteNames]

-- Sharp indices starting from C
sharps = S.fromList [1, 3, 6, 8, 10]

isSharp :: Pitch -> Bool
isSharp (Pitch p) = (p `mod` 12) `S.member` sharps

middleC = centreNoteOfKey (Key 0)

-- Number of semitones to shift original *up* to reach target
semitonesBetweenKeys :: Key -> Key -> Int
semitonesBetweenKeys (Key target) (Key original) = (target - original) `mod` 12

-- Number of semitones to shift original either up or down to reach target
semitonesBetweenKeys' :: Key -> Key -> Int
semitonesBetweenKeys' (Key target) (Key original) = (target - original + 6) `mod` 12 - 6

-- Number of semitones between two pitches
semitonesBetweenPitches :: Pitch -> Pitch -> Int
semitonesBetweenPitches (Pitch target) (Pitch original) = target - original

-- Number of semitones to move to be within [-11..0] semitones of target
shiftToBelowNote :: Pitch -> Pitch -> Int
shiftToBelowNote target p = 12 * octaveShift
    where octaveShift = semitonesBetweenPitches target p `div` 12

adjustPitchBySemitones :: Int -> Pitch -> Pitch
adjustPitchBySemitones s (Pitch p) = pitchFromMidiValue $ p + s

-- Move between 7 and -6 semitones for key
normaliseToKey :: Key -> Key -> Pitch -> Pitch
normaliseToKey targetKey originalKey = adjustPitchBySemitones s
    where s = semitonesBetweenKeys' targetKey originalKey

centreNoteOfKey (Key key) = Pitch $ key + 5*12

keyFromSemitonesFromC :: Int -> Key
keyFromSemitonesFromC n = Key $ n `mod` 12

pitchFromMidiValue :: Int -> Pitch
pitchFromMidiPitch :: Voice.Pitch -> Pitch
pitchToMidiValue   :: Pitch -> Int

pitchFromMidiValue = Pitch
pitchFromMidiPitch = pitchFromMidiValue . Voice.fromPitch
pitchToMidiValue (Pitch p) = p

fancify   = map (\case '#' -> '♯'; 'b' -> '♭'; c -> c)
unfancify = map (\case '♯' -> '#'; '♭' -> 'b'; c -> c)

-- Take a MIDI pitch value (between 0 and 127) and return name like "C0", "Db5"
pitchToSPN :: Pitch -> String
pitchToSPN (Pitch p) = noteName ++ show (octaveNumber - 1)

    where noteName =  noteNames !! noteNumber
          (octaveNumber, noteNumber) = p `divMod` 12


-- pitchToName using unicode sharp and flat characters
pitchToSPN' :: Pitch -> String
pitchToSPN' = fancify . pitchToSPN

(<+>) = liftM2 (+)

-- Return number for key name ("C" = 0, "C#" = "Db" = 1)
keyFromName :: String -> Maybe Key
keyFromName = fmap Key . f . map toUpper
    where f [n, a] = accidentalValue a <+> noteValue n
          f [n] = noteValue n
          f _ = Nothing

-- Take a name like "C0", "Db5" and return its MIDI pitch value
pitchFromName :: String -> Maybe Pitch
pitchFromName = fmap Pitch . f . map toUpper
    where f [n, a, o] = accidentalValue a <+> f [n, o]
          f [n, o] = noteValue n <+> octaveValue o
          f _ = Nothing

-- pitchFromName using unicode sharp and flat characters
pitchFromName' :: String -> Maybe Pitch
pitchFromName' = pitchFromName . unfancify

noteValue :: Char -> Maybe Int
noteValue n = elemIndex [toUpper n] noteNames

octaveValue :: Char -> Maybe Int
octaveValue c = do
    n <- readMaybe [c]
    return $ (n+1) * 12

accidentalValue :: Char -> Maybe Int
accidentalValue a = case toUpper a of
                      'b' -> Just (-1)
                      '#' -> Just 1
                      _   -> Nothing
