module Solver.Term exposing
  ( Term
  , constant

  , evaluate
  )

{-| Individual terms found in constraints.
-}

import Solver.Range exposing (Range)

import EveryDict exposing (EveryDict)


{-| A term in a constraint.
-}
type Term var
  = Constant Int


{-| A constant value.
-}
constant : Int -> Term var
constant =
  Constant


{-| Evaluate the range of vaues a term can have, given a set of known
variable ranges.
-}
evaluate : EveryDict var Range -> Term var -> Range
evaluate _ term =
  case term of
    Constant value ->
      Solver.Range.singleton value
