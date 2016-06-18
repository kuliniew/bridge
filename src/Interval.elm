module Interval exposing
  ( Interval
  , empty
  , singleton
  , fromSet
  , toSet
  , member

  , add
  , subtract
  , multiply
  , divide
  , min
  , max
  , negate

  , intersect
  , union

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


{-| Compute the interval formed by multiplying all possible pairs between
the elements of two intervals.
-}
multiply : Interval -> Interval -> Interval
multiply interval1 interval2 =
  case (interval1, interval2) of
    (Interval lo1 hi1, Interval lo2 hi2) ->
      let
        products = [lo1 * lo2, lo1 * hi2, hi1 * lo2, hi1 * hi2]
      in
        case (List.minimum products, List.maximum products) of
          (Just lo, Just hi) -> Interval lo hi
          _ -> Debug.crash "the list was supposed to be non-empty"
    _ -> Empty


{-| Compute the interval formed by dividing all possible pairs between
the elements of two intervals, using integer division.
-}
divide : Interval -> Interval -> Interval
divide interval1 interval2 =
  let
    filterPositive iv =
      case iv of
        Interval lo hi ->
          if hi < 0
          then Empty
          else Interval (Basics.max lo 1) hi
        Empty ->
          Empty
    filterNegative iv =
      case iv of
        Interval lo hi ->
          if lo > 0
          then Empty
          else Interval lo (Basics.min hi (-1))
        Empty ->
          Empty
    oneSideDivide iv1 iv2 =
      case (iv1, iv2) of
        (Interval lo1 hi1, Interval lo2 hi2) ->
          let
            quotients = [lo1 // lo2, lo1 // hi2, hi1 // lo2, hi1 // hi2]
          in
            case (List.minimum quotients, List.maximum quotients) of
              (Just lo, Just hi) -> Interval lo hi
              _ -> Empty
        _ -> Empty
  in
    union
      (oneSideDivide interval1 (filterPositive interval2))
      (oneSideDivide interval1 (filterNegative interval2))


{-| Compute the interval formed by taking the minimum value of all possible
pairs between the elements of two intervals.
-}
min : Interval -> Interval -> Interval
min interval1 interval2 =
  case (interval1, interval2) of
    (Interval lo1 hi1, Interval lo2 hi2) -> Interval (Basics.min lo1 lo2) (Basics.min hi1 hi2)
    _ -> Empty


{-| Compute the interval formed by taking the maximum value of all possible
pairs between the elements of two intervals.
-}
max : Interval -> Interval -> Interval
max interval1 interval2 =
  case (interval1, interval2) of
    (Interval lo1 hi1, Interval lo2 hi2) -> Interval (Basics.max lo1 lo2) (Basics.max hi1 hi2)
    _ -> Empty


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
        lo = Basics.max lo1 lo2
        hi = Basics.min hi1 hi2
      in
        if lo <= hi
        then Interval lo hi
        else Empty
    _ ->
      Empty


{-| Compute the union of two intervals.
-}
union : Interval -> Interval -> Interval
union interval1 interval2 =
  case (interval1, interval2) of
    (Interval lo1 hi1, Interval lo2 hi2) ->
      Interval (Basics.min lo1 lo2) (Basics.max hi1 hi2)
    (Empty, _) ->
      interval2
    (_, Empty) ->
      interval1


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
  Interval (Basics.min x y) (Basics.max x y)


{-| Deterministically pick a sample element from a non-empty interval and
include it in the result.
-}
addSampleElement : Interval -> (Interval, Int)
addSampleElement interval =
  case interval of
    Interval lo hi -> (interval, Random.Extra.quickGenerate <| Random.int lo hi)
    Empty -> Debug.crash "can't take a sample element from an empty interval"
