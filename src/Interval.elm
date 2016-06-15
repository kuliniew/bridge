module Interval exposing
  ( Interval
  , empty
  , singleton
  , fromSet
  , toSet
  , member

  , add
  , subtract
  , negate

  , intersect

  , producer
  , producerWithElement
  )

{-| A possibly-empty interval between two integers which includes both its
endpoints.
-}

import Check.Producer
import List.Extra
import Random
import Random.Extra
import Set exposing (Set)
import Shrink


{-| A bounded interval.
-}
type Interval
  = Interval Int Int
  | Empty


{-| An empty interval.
-}
empty : Interval
empty = Empty


{-| An interval containing a single value.
-}
singleton : Int -> Interval
singleton val = Interval val val


{-| Create the smallest possible interval that includes all of the members
of a set.
-}
fromSet : Set Int -> Interval
fromSet set =
  let
    list = Set.toList set
  in
    case (List.head list, List.Extra.last list) of
      (Just lo, Just hi) -> Interval lo hi
      _ -> Empty


{-| Create a set that contains the same values as the interval.
-}
toSet : Interval -> Set Int
toSet interval =
  case interval of
    Interval lo hi -> Set.fromList [lo .. hi]
    Empty -> Set.empty


{-| Test if a value is inside the interval.
-}
member : Int -> Interval -> Bool
member val interval =
  case interval of
    Interval lo hi -> lo <= val && val <= hi
    Empty -> False


{-| Compute the interval formed by adding all possible pairs between the
elements of two intervals.
-}
add : Interval -> Interval -> Interval
add interval1 interval2 =
  case (interval1, interval2) of
    (Interval lo1 hi1, Interval lo2 hi2) -> Interval (lo1 + lo2) (hi1 + hi2)
    _ -> Empty


{-| Compute the interval formed by subtracting all possible values in the
second interval from all possible values in the first interval.
-}
subtract : Interval -> Interval -> Interval
subtract interval1 interval2 =
  add interval1 (negate interval2)


{-| Negate the values in an interval.
-}
negate : Interval -> Interval
negate interval =
  case interval of
    Interval lo hi -> Interval (Basics.negate hi) (Basics.negate lo)
    Empty -> Empty


{-| Compute the intersection of two intervals.
-}
intersect : Interval -> Interval -> Interval
intersect interval1 interval2 =
  case (interval1, interval2) of
    (Interval lo1 hi1, Interval lo2 hi2) ->
      let
        lo = max lo1 lo2
        hi = min hi1 hi2
      in
        if lo <= hi
        then Interval lo hi
        else Empty
    _ ->
      Empty


{-| Producer for arbitrary intervals.
-}
producer : Check.Producer.Producer Interval
producer =
  { generator = Random.Extra.merge emptyGenerator nonEmptyGenerator
  , shrinker = shrinker
  }


{-| Producer for non-empty intervals with a sample element.
-}
producerWithElement : Check.Producer.Producer (Interval, Int)
producerWithElement =
  { generator = Random.map addSampleElement nonEmptyGenerator
  , shrinker = fst >> shrinker >> Shrink.map addSampleElement
  }


{-| Generate an empty interval.
-}
emptyGenerator : Random.Generator Interval
emptyGenerator =
  Random.Extra.constant empty


{-| Generate a non-empty interval.
-}
nonEmptyGenerator : Random.Generator Interval
nonEmptyGenerator =
  let
    endpointGenerator =
      Random.int (-100) 100
  in
    Random.map2 fromUnorderedEndpoints endpointGenerator endpointGenerator


{-| Shrink an interval.
-}
shrinker : Shrink.Shrinker Interval
shrinker interval =
  case interval of
    Interval lo hi ->
      fromUnorderedEndpoints `Shrink.map` Shrink.int lo `Shrink.andMap` Shrink.int hi
    Empty ->
      Shrink.noShrink interval


{-| Create an interval from its endpoints, which may be out of order.
-}
fromUnorderedEndpoints : Int -> Int -> Interval
fromUnorderedEndpoints x y =
  Interval (min x y) (max x y)


{-| Deterministically pick a sample element from a non-empty interval and
include it in the result.
-}
addSampleElement : Interval -> (Interval, Int)
addSampleElement interval =
  case interval of
    Interval lo hi -> (interval, Random.Extra.quickGenerate <| Random.int lo hi)
    Empty -> Debug.crash "can't take a sample element from an empty interval"
