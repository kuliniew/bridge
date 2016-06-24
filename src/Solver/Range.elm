module Solver.Range exposing
  ( Range
  , empty
  , full
  , member
  , isEmpty
  )

{-| Operations over discontinuous unbounded, potentially-empty ranges
of integers.
-}

import Solver.Interval exposing (Interval)


{-| A range of integers.  This is conceptually any subset of integers,
although the implementation is most efficient if the range consists of
only a small number of continuous intervals.
-}
type Range
  = Range (List Interval)


{-| The empty range.
-}
empty : Range
empty =
  Range []


{-| The full range, containing all integers.
-}
full : Range
full =
  Range [Solver.Interval.unbounded]


{-| Test if an integer belongs to the range.
-}
member : Int -> Range -> Bool
member value (Range intervals) =
  List.any (Solver.Interval.member value) intervals


{-| Test if a range is empty.
-}
isEmpty : Range -> Bool
isEmpty (Range intervals) =
  List.isEmpty intervals
