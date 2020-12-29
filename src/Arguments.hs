{-# LANGUAGE ScopedTypeVariables #-}
module Arguments
    ( ProgramOptions(..)
    , programOptions
    , parseToMaybe
    ) where

import qualified System.FilePath as Path

import EqualTemperament (Key, Pitch)
import Options.Applicative
import Text.Read (readMaybe)

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
  , xticks :: Bool
  , yticks :: Bool
  , tight :: Bool
  , yScale :: Double
  }

invSwitch = fmap not . switch

optionParser :: Parser ProgramOptions
optionParser = ProgramOptions
    <$> strArgument (
           help "input midi file"
        <> metavar "INPUT-FILE" )
    <*> argument auto (
           help "input key of piece"
        <> metavar "INPUT-KEY" )
    <*> option (maybeReader $ Just . Just) (
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
    <*> option (maybeReader parseToMaybe) (
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
        <> value (read "C3")
        <> showDefault
        <> help "lowest pitch to plot" )
    <*> option auto (
           long "max-pitch"
        <> value (read "C5")
        <> showDefault
        <> help "highest pitch to plot" )
    <*> option auto (
           long "output-width"
        <> short 'w'
        <> value 1000
        <> showDefault
        <> help "width of output image" )
    <*> invSwitch (
           long "no-xticks"
        <> help "do not show bar numbers along X axis" )
    <*> switch (
           long "yticks"
        <> help "show pitch names in SPN along Y axis" )
    <*> switch (
           long "tight"
        <> help "graph will tightly fit image" )
    <*> option auto (
           long "yscale"
        <> value 0.1
        <> showDefault
        <> help "height of individual pitch line" )

parseToMaybe :: (Read a) => String -> Maybe (Maybe a)
parseToMaybe = fmap Just . readMaybe

programOptions :: ParserInfo ProgramOptions
programOptions = info (optionParser <**> helper) (
    progDesc "Modify the key of a MIDI file and plot the result")
