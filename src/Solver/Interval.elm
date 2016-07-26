module Solver.Interval exposing
  ( Interval
  , empty
  , unbounded
  , singleton
  , fromLowerBound
  , fromUpperBound
  , fromEndpoints
  , toEndpoints

  , isEmpty
  , member
  , subset

  , removeLowerBound
  , removeUpperBound

  , add
  , negate
  , subtract
  , multiply
  , divide
  , intersect
  , hull
  , union
  )

{-| Operations over continuous, potentially-unbounded integer intervals.
-}

import Solver.Endpoint exposing (Endpoint)


{-| A continuous integer interval that contains all integers between its
two (optional) endpoints, inclusive.  An interval may be empty, which
makes modeling operations over intervals much easier.
-}
type Interval
  = NonEmpty Endpoint Endpoint
  | Empty


{-| The interval containing no integers.
-}
empty : Interval
empty =
  Empty


{-| The interval containing all integers.
-}
unbounded : Interval
unbounded =
  NonEmpty Solver.Endpoint.NegativeInfinity Solver.Endpoint.PositiveInfinity


{-| The interval containing a single integer.
-}
singleton : Int -> Interval
singleton value =
  let
    point =
      Solver.Endpoint.Point value
  in
    NonEmpty point point


{-| The interval with a given lower bound but no upper bound.
-}
fromLowerBound : Int -> Interval
fromLowerBound bound =
  NonEmpty (Solver.Endpoint.Point bound) Solver.Endpoint.PositiveInfinity


{-| The interval with a given upper bound but no lower bound.
-}
fromUpperBound : Int -> Interval
fromUpperBound bound =
  NonEmpty Solver.Endpoint.NegativeInfinity (Solver.Endpoint.Point bound)


{-| The interval bounded by the given endpoints, if the endpoints form
a valid interval.
-}
fromEndpoints : Endpoint -> Endpoint -> Maybe Interval
fromEndpoints lo hi =
  if lo `Solver.Endpoint.lessThanOrEqual` hi &&
     lo /= Solver.Endpoint.PositiveInfinity &&
     hi /= Solver.Endpoint.NegativeInfinity
  then Just (NonEmpty lo hi)
  else Nothing


{-| The endpoints of a non-empty interval.
-}
toEndpoints : Interval -> Maybe (Endpoint, Endpoint)
toEndpoints interval =
  case interval of
    NonEmpty lo hi -> Just (lo, hi)
    Empty -> Nothing


{-| Check if an interval is empty.
-}
isEmpty : Interval -> Bool
isEmpty interval =
  case interval of
    NonEmpty _ _ -> False
    Empty -> True


{-| Check if an interval contains a value.
-}
member : Int -> Interval -> Bool
member value interval =
  case interval of
    NonEmpty lo hi ->
      lo `Solver.Endpoint.lessThanOrEqual` Solver.Endpoint.Point value &&
      Solver.Endpoint.Point value `Solver.Endpoint.lessThanOrEqual` hi
    Empty ->
      False


{-| Check if all of the values in one interval are contained by another
interval.
-}
subset : Interval -> Interval -> Bool
subset left right =
  case (left, right) of
    (NonEmpty leftLo leftHi, NonEmpty rightLo rightHi) ->
      rightLo `Solver.Endpoint.lessThanOrEqual` leftLo && leftHi `Solver.Endpoint.lessThanOrEqual` rightHi
    (Empty, _) ->
      True
    (_, Empty) ->
      False


{-| Remove the lower bound from an interval, such that the new interval will
contain all the values less than or equal to any element of the original
interval.
-}
removeLowerBound : Interval -> Interval
removeLowerBound interval =
  case interval of
    NonEmpty _ hi ->
      NonEmpty Solver.Endpoint.NegativeInfinity hi
    Empty ->
      Empty


{-| Remove the upper bound from an interval, such that the new interval will
contain all the values greater than or equal to any element of the original
interval.
-}
removeUpperBound : Interval -> Interval
removeUpperBound interval =
  case interval of
    NonEmpty lo _ ->
      NonEmpty lo Solver.Endpoint.PositiveInfinity
    Empty ->
      Empty


{-| Compute the sum of two intervals.
-}
add : Interval -> Interval -> Interval
add interval1 interval2 =
  case (interval1, interval2) of
    (NonEmpty lo1 hi1, NonEmpty lo2 hi2) ->
      let
        addOrCrash ep1 ep2 =
          case ep1 `Solver.Endpoint.add` ep2 of
            Just value -> value
            Nothing -> Debug.crash <| "tried to add " ++ toString ep1 ++ " and " ++ toString ep2 ++ " during interval addition"
      in
        NonEmpty (lo1 `addOrCrash` lo2) (hi1 `addOrCrash` hi2)
    _ ->
      Empty


{-| Negate an interval.
-}
negate : Interval -> Interval
negate interval =
  case interval of
    NonEmpty lo hi ->
      NonEmpty (Solver.Endpoint.negate hi) (Solver.Endpoint.negate lo)
    Empty ->
      Empty


{-| Compute the difference of two intervals.
-}
subtract : Interval -> Interval -> Interval
subtract interval1 interval2 =
  add interval1 (negate interval2)


{-| Multiply an interval by a constant.
-}
multiply : Int -> Interval -> Interval
multiply coeff interval =
  case interval of
    NonEmpty lo hi ->
      if coeff > 0
      then NonEmpty (Solver.Endpoint.multiply coeff lo) (Solver.Endpoint.multiply coeff hi)
      else
        if coeff < 0
        then NonEmpty (Solver.Endpoint.multiply coeff hi) (Solver.Endpoint.multiply coeff lo)
        else NonEmpty (Solver.Endpoint.Point 0) (Solver.Endpoint.Point 0)
    Empty ->
      Empty


{-| Divide an interval by a constant.
-}
divide : Interval -> Int -> Interval
divide interval divisor =
  case interval of
    NonEmpty lo hi ->
      case (Solver.Endpoint.divide lo divisor, Solver.Endpoint.divide hi divisor) of
        (Just loQuotient, Just hiQuotient) ->
          if divisor > 0
          then NonEmpty loQuotient hiQuotient
          else NonEmpty hiQuotient loQuotient
        _ ->
          Empty
    Empty ->
      Empty


{-| Compute the values that appear in both intervals.
-}
intersect : Interval -> Interval -> Interval
intersect interval1 interval2 =
  case (interval1, interval2) of
    (NonEmpty lo1 hi1, NonEmpty lo2 hi2) ->
      let
        lo =
          Solver.Endpoint.max lo1 lo2
        hi =
          Solver.Endpoint.min hi1 hi2
      in
        if lo `Solver.Endpoint.lessThanOrEqual` hi
        then NonEmpty lo hi
        else Empty
    _ ->
      Empty


{-| Compute the hull of two intervals: the smallest interval that contains
all of the elements in either interval.  Note that this in many cases will
include elements not in either interval, if there are values "between" the
two input intervals.
-}
hull : Interval -> Interval -> Interval
hull interval1 interval2 =
  case (interval1, interval2) of
    (NonEmpty lo1 hi1, NonEmpty lo2 hi2) ->
      let
        lo =
          Solver.Endpoint.min lo1 lo2
        hi =
          Solver.Endpoint.max hi1 hi2
      in
        NonEmpty lo hi
    (Empty, _) ->
      interval2
    (_, Empty) ->
      interval1


{-| Compute the union of two intervals, but only if such an interval exists.
The union includes exactly the elements that appear in at least one of the
input intervals, and no others.  The union only exists if two intervals are
overlapping or immediately adjacent.
-}
union : Interval -> Interval -> Maybe Interval
union interval1 interval2 =
  let
    overlapping =
      not <| isEmpty <| intersect interval1 interval2
    adjacent =
      case (interval1, interval2) of
        (NonEmpty lo1 hi1, NonEmpty lo2 hi2) ->
          Solver.Endpoint.adjacent hi1 lo2 || Solver.Endpoint.adjacent hi2 lo1
        (Empty, _) ->
          True
        (_, Empty) ->
          True
  in
    if overlapping || adjacent
    then Just <| hull interval1 interval2
    else Nothing
