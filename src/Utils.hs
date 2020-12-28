{-# LANGUAGE RecordWildCards #-}

module Utils
  ( bimapBoth
  , limToRange
  , ignoreClip
  , clipDataXRange
  , mapRanges
  , mapRangeKeys
  , ratioToFrac
  ) where

import qualified Data.Map as M
import Data.Bifunctor
import Types

bimapBoth f = bimap f f

clipDataXRange :: (Ord k, Num a, Ord a)
               => RangeData k a
               -> Range a
               -> RangeData k (Clipped a)

clipDataXRange d (x0, x1) = M.map (map clip . filter inBounds) d
    where inBounds = uncurry (&&) . bimap (<= x1) (>= x0)
          clip = bimap (\x -> if x < x0 then ClipLeft x0 else Unclipped x)
                       (\x -> if x > x1 then ClipRight x1 else Unclipped x)

ignoreClip :: Clipped x -> x
ignoreClip (ClipLeft x) = x
ignoreClip (ClipRight x) = x
ignoreClip (Unclipped x) = x

limToRange :: Num a => AxisLimit a -> (a, a)
limToRange CentreAt {..} = (y - minDY, y + minDY)
limToRange (AxisRange x) = x

mapRanges :: (v -> v') -> RangeData k v -> RangeData k v'
mapRanges f = M.map $ map $ bimapBoth f

mapRangeKeys :: (Ord k, Ord k') => (k -> k') -> RangeData k v -> RangeData k' v
mapRangeKeys = M.mapKeys

ratioToFrac :: (Real a, Fractional b) => a -> b
ratioToFrac = fromRational . toRational
