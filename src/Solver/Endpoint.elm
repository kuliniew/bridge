module Solver.Endpoint exposing
  ( Endpoint (..)
  , lessThanOrEqual
  , lessThan
  , greaterThanOrEqual
  , greaterThan
  , compare
  , min
  , max

  , adjacent
  , add
  , negate
  )

{-| Operations over endpoints of potentially-unbounded integer intervals.
-}


{-| One logical endpoint of an integer interval.  "Points" at negative
and positive infinity are used to represent the lack of a lower and
upper bound, respectively.
-}
type Endpoint
  = NegativeInfinity
  | Point Int
  | PositiveInfinity


{-| Check whether one endpoint <= another endpoint.
-}
lessThanOrEqual : Endpoint -> Endpoint -> Bool
lessThanOrEqual left right =
  case (left, right) of
    (NegativeInfinity, _) ->
      True
    (_, PositiveInfinity) ->
      True
    (Point leftValue, Point rightValue) ->
      leftValue <= rightValue
    _ ->
      False


{-| Check whether one endpoint < another endpoint.
-}
lessThan : Endpoint -> Endpoint -> Bool
lessThan left right =
  left `lessThanOrEqual` right && left /= right


{-| Check whether one endpoint >= another endpoint.
-}
greaterThanOrEqual : Endpoint -> Endpoint -> Bool
greaterThanOrEqual =
  flip lessThanOrEqual


{-| Check whether one endpoint > another endpoint.
-}
greaterThan : Endpoint -> Endpoint -> Bool
greaterThan =
  flip lessThan


{-| Compare two endpoints for sorting purposes.
-}
compare : Endpoint -> Endpoint -> Order
compare left right =
  if left `lessThan` right
  then LT
  else
    if left `greaterThan` right
    then GT
    else EQ


{-| Choose the lesser of two endpoints.
-}
min : Endpoint -> Endpoint -> Endpoint
min endpoint1 endpoint2 =
  if endpoint1 `lessThanOrEqual` endpoint2
  then endpoint1
  else endpoint2


{-| Choose the greater of two endpoints.
-}
max : Endpoint -> Endpoint -> Endpoint
max endpoint1 endpoint2 =
  if endpoint1 `greaterThanOrEqual` endpoint2
  then endpoint1
  else endpoint2


{-| Test if two points are adjacent to one another.
-}
adjacent : Endpoint -> Endpoint -> Bool
adjacent endpoint1 endpoint2 =
  case (endpoint1, endpoint2) of
    (Point value1, Point value2) ->
      value1 + 1 == value2
    _ ->
      False


{-| Add two endpoints together.

The sum of NegativeInfinity and PositiveInfinity is undefined, since
(1) it doesn't make sense in terms of interval endpoints, and (2) any
value that it could be used as the result breaks associativity in the
case of "NegativeInfinity + PositiveInfinity + x".
-}
add : Endpoint -> Endpoint -> Maybe Endpoint
add endpoint1 endpoint2 =
  case (endpoint1, endpoint2) of
    (Point value1, Point value2) ->
      Just <| Point (value1 + value2)
    (PositiveInfinity, NegativeInfinity) ->
      Nothing
    (NegativeInfinity, PositiveInfinity) ->
      Nothing
    (PositiveInfinity, _) ->
      Just PositiveInfinity
    (_, PositiveInfinity) ->
      Just PositiveInfinity
    (NegativeInfinity, _) ->
      Just NegativeInfinity
    (_, NegativeInfinity) ->
      Just NegativeInfinity


{-| Negate a point.

Negation is also the additive inverse operation.
-}
negate : Endpoint -> Endpoint
negate endpoint =
  case endpoint of
    Point value ->
      Point (Basics.negate value)
    PositiveInfinity ->
      NegativeInfinity
    NegativeInfinity ->
      PositiveInfinity
