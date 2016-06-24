module Solver.Endpoint exposing
  ( Endpoint (..)
  , lessThanOrEqual
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
