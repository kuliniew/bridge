module Solver.Interval exposing
  ( Interval
  , empty
  , unbounded
  , member
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
