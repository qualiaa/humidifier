{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module PianoRoll
  ( pianoRoll
  , PlotSettings(..)
  ) where

import qualified Data.Map as M

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import EqualTemperament (isSharp, pitchFromMidiValue)
import Types
import Utils


data PlotSettings = PlotSettings { showXTicks :: Bool
                                 , showYTicks :: Bool
                                 , tight :: Bool
                                 , yScale :: Double
                                 }

-- TODO Generalise for different time signatures
-- TODO Check that when sx is 0 the separation is right
beatLines :: Double -> Double -> Double -> Diagram B
beatLines x0 x1 h = hsep 1 lines # translateX (1+sx) # scaleX (1/4)
    where lines = zipWith ($) lineStyles'
                              (replicate numBeats (vrule h # alignB))
          x0' = ceiling (x0 * beatUnit)
          x1' = floor   (x1 * beatUnit)
          sx = fromIntegral x0' - (x0 * beatUnit)
          numBeats = x1' - x0'
          beatUnit = 4
          beatsPerBar = 4

          -- Thin beats, thick bars
          beatStyle = lc grey . lw veryThin . dashingN [0.002, 0.003] 0
          barStyle = lw thin
          lineStyles = cycle $ replicate (beatsPerBar-1) beatStyle ++ [barStyle]
          -- Drop the beats *dab*
          lineStyles' = drop (x0' `mod` beatsPerBar) lineStyles

note :: Double -> Diagram B
note len = unitSquare # translateX 0.5 # scaleX len

pitchLine :: [(Clipped Double, Clipped Double)] -> Diagram B
pitchLine spans = position [(p2 (x0, 0), note $ x1 - x0)
                           | (x0, x1) <- spans']

    where spans' = map (bimapBoth ignoreClip) spans

-- Totally hard-coded graph for notes by pitch
pianoRoll :: RangeData Int Double
          -> Int
          -> AxisLimit Double
          -> AxisLimit Int
          -> PlotSettings
          -> Diagram B
pianoRoll pitchNotes beatsPerBar xlim ylim PlotSettings{..} = g

    where -- Input clipped to given x-range
          pitchNotes' = clipDataXRange pitchNotes (x0, x1)

          -- Diagram showing clipped data
          pitchLinesAbsShapes = position [
                (p2 (0, fromIntegral y), pitchLine xBars)
                | (y, xBars) <- M.toList pitchNotes'] # translateY 0.5

          pitchLinesAbs = pitchLinesAbsShapes # lw none # fc orange # opacity 0.5
                       <> pitchLinesAbsShapes # lw thin

          -- Bounds and width of graph contents (excluding empty margins)
          Just (minY, maxY) = extentY pitchLinesAbs

          (w, h) = (x1 - x0, y1 - y0)

          (x0, x1) = limToRange xlim
          (y0', y1') = limToRange ylim

          y0 = max 0 $ min minY (fromIntegral y0')
          y1 = max maxY (fromIntegral y1')

          xs = [ceiling x0 .. floor x1] :: [Int]
          ys = [round y0.. round y1 - 1] :: [Int]

          -- Grid of beats and bars limited to drawn region
          -- numBeats = round $ w * fromIntegral beatsPerBar
          grid = beatLines x0 x1 h

          -- Shade in accidental pitches
          sharpPitches = [pitch | pitch <- ys, isSharp $ pitchFromMidiValue pitch]

          sharpShading = position [(p2 (0, fromIntegral pitch),

                                    rect w 1 # fc black
                                             # lw none
                                             # opacity 0.2)

                                  | pitch <- sharpPitches]
              # alignL # translateY (0.5 - y0)

          -- Lines demarking octaves
          octavePitches = filter (\y -> y `mod` 12 == 0) ys
          octaveLines = position [(p2 (0, fromIntegral pitch), hrule w)
                                 | pitch <- octavePitches]
              # alignL # translateY (-y0)

          -- The graph contents translated (x0, y1) -> (0,0), in the bottom left
          pitchLinesRel = pitchLinesAbs # translate (r2 (-x0, -y0))

          -- background = rect (x1 - x0) (y1 - y0)
          --       # fillColor indianred
          --       # lw none
          --       # alignBL

          graphContents = (  pitchLinesRel
                          <> octaveLines
                          <> grid
                          <> sharpShading
                          ) # scaleY yScale
              -- <> background

          -- Axis labels
          pitchNames = map (show . pitchFromMidiValue) ys
          pitchLabels = position [(p2 (-0.5, fromIntegral y * yScale), text n)
                                 | (y, n) <- zip ys pitchNames]
              # alignB
              # translateY (0.5*yScale)

          barTicks = mconcat [p2 (x', 0) ~~ p2 (x', -0.075)
                             | x <- xs, let x' = fromIntegral x]
              # alignL
              # lw thin :: Diagram B
          barLabels = alignL $ position [
              (p2 (fromIntegral x, -0.20), text $ show x) | x <- xs] :: Diagram B


          graphAxes = vrule h # alignB # scaleY yScale # lw thick
                   <> mwhen showYTicks (
                         pitchLabels # fontSizeN 0.015)
                   <> mwhen showXTicks (
                         (barTicks <> barLabels) # fontSizeN 0.02)

          g = (graphAxes <> graphContents) # if tight then id else frame 0.25
