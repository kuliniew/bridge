module Solver.Range exposing
  ( Range
  , empty
  , full
  , singleton
  , fromIntervals
  , toIntervals

  , member
  , isEmpty
  , subset

  , intersect
  , union
  )

{-| Operations over discontinuous unbounded, potentially-empty ranges
of integers.
-}

import Solver.Endpoint
import Solver.Interval exposing (Interval)

import List.Extra


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


{-| A range containing a single value.
-}
singleton : Int -> Range
singleton value =
  Range [Solver.Interval.singleton value]


{-| A range that includes all the elements of each interval.
-}
fromIntervals : List Interval -> Range
fromIntervals intervals =
  let
    sortedIntervals =
      List.filterMap Solver.Interval.toEndpoints intervals
        |> List.sortWith (\(lo1, _) (lo2, _) -> Solver.Endpoint.compare lo1 lo2)
        |> List.filterMap (uncurry Solver.Interval.fromEndpoints)
    coalesce intervals =
      case intervals of
        interval1 :: interval2 :: rest ->
          case Solver.Interval.union interval1 interval2 of
            Just interval -> coalesce (interval :: rest)
            Nothing -> interval1 :: coalesce (interval2 :: rest)
        [_] ->
          intervals
        [] ->
          intervals
  in
    Range (coalesce sortedIntervals)


{-| Decompose a range into its component intervals.
-}
toIntervals : Range -> List Interval
toIntervals (Range intervals) =
  intervals


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


{-| Test if the first range is a subset of the second.
-}
subset : Range -> Range -> Bool
subset left right =
  intersect left right == left


{-| Compute the values that appear in both ranges.
-}
intersect : Range -> Range -> Range
intersect (Range intervals1) (Range intervals2) =
  let
    components =
      intervals1 `List.Extra.andThen` \interval1 ->
        intervals2 `List.Extra.andThen` \interval2 ->
          [Solver.Interval.intersect interval1 interval2]
  in
    fromIntervals components


{-| Compute the values that appear in either range.
-}
union : Range -> Range -> Range
union (Range intervals1) (Range intervals2) =
  fromIntervals (intervals1 ++ intervals2)
