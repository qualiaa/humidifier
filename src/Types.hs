module Types
  ( AxisLimit(..)
  , Clipped(..)
  , RangeData(..)
  , Range(..)
  ) where

import qualified Data.Map as M

type Range a = (a, a)
type RangeData k a = M.Map k [Range a]

data Clipped a = ClipLeft a | ClipRight a | Unclipped a deriving (Show, Eq)

data AxisLimit a = CentreAt { y     :: a
                            , minDY :: a }
                 | AxisRange (Range a)
                 deriving Show
