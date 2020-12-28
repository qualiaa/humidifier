{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}

module PianoRoll
  ( pianoRoll
  ) where

import qualified Data.Map as M

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

import EqualTemperament (isSharp, pitchFromMidiValue)
import Types
import Utils

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
          -> Diagram B
pianoRoll pitchNotes beatsPerBar xlim ylim = g

    where -- Input clipped to given x-range
          pitchNotes' = clipDataXRange pitchNotes (x0, x1)

          -- Diagram showing clipped data
          pitchLinesAbs = position [
                (p2 (0, fromIntegral y), pitchLine xBars)
                | (y, xBars) <- M.toList pitchNotes'] # lw none # fc black # translateY 0.5

          -- Bounds and width of graph contents (excluding empty margins)
          (Just (minX, maxX), Just (minY, maxY)) = (extentX pitchLinesAbs,
                                                    extentY pitchLinesAbs)
          --(w, h) = (maxX - minX, maxY - minY)
          (w, h) = (x1 - x0, y1 - y0)

          -- The vertical bounds of the graph area
          (x0, x1) = limToRange xlim
          (y0', y1') = limToRange ylim

          y0 = max 0 $ min minY (fromIntegral y0')
          y1 = max maxY (fromIntegral y1')
          ys = [round y0.. round y1 - 1] :: [Int]

          -- Grid of beats and bars limited to drawn region
          numBeats = round $ w * fromIntegral beatsPerBar
          grid = beatLines x0 x1 h

          -- Shade in accidental pitches
          sharpPitches = [pitch | pitch <- ys, isSharp $ pitchFromMidiValue pitch]

          sharpShading = position [(p2 (0, fromIntegral pitch),

                                    rect w 1 # fillColor black
                                             # lw none
                                             # opacity 0.2)

                                  | pitch <- sharpPitches]
              # alignL # translateY (0.5 - y0)

          -- Lines demarking octaves
          octavePitches = filter (\y -> y `mod` 12 == 0) ys
          octaveLines = position [(p2 (0, fromIntegral pitch), hrule (x1-x0))
                                 | pitch <- octavePitches]
              # alignL # translateY (-y0)

          -- The graph contents translated (x0, y1) -> (0,0), in the bottom left
          pitchLinesRel = pitchLinesAbs # translate (r2 (-x0, -y0))

          -- background = rect (x1 - x0) (y1 - y0)
          --       # fillColor indianred
          --       # lw none
          --       # alignBL

          graphContents = pitchLinesRel
              <> sharpShading
              <> octaveLines
              <> grid
              -- <> background

          -- Axis labels
          pitchNames = map (show . pitchFromMidiValue) ys
          pitchPositions = [p2 (-1.5, fromIntegral y) | y <- ys]
          pitchLabels = translateY 0.5 . alignB . position $
              zip pitchPositions (map text pitchNames) :: Diagram B

          xs = [ceiling x0 .. floor x1] :: [Int]
          barLabels = alignL $ position [
              (p2 (fromIntegral x, -0.75), text $ show x) | x <- xs] :: Diagram B


          graphAxes = (vrule (y1-y0) # alignB {- <> hrule (x1-x0) # alignL-}) # lw thick
              <> (barLabels <> pitchLabels) # fontSizeL 0.6

          g = (graphAxes  <> graphContents) # frame 1.5
