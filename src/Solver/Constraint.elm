module Solver.Constraint exposing
  ( Constraint
  , equal

  , evaluate
  , boundVariables
  )

{-| Integer constraints over variables.
-}

import Solver.Range exposing (Range)
import Solver.Term exposing (Term)

import EveryDict exposing (EveryDict)



{-| A constraint to be satisfied.

Whenever a constraint implements a relation R, the interpretation is
"a R b".  For example, LessThan a b would be "a is less than b".
-}
type Constraint var
  = Equal (Term var) (Term var)


{-| Constrain two terms to be exactly equal.
-}
equal : Term var -> Term var -> Constraint var
equal = Equal


{-| Evaluate a constraint over a set of known variable ranges, returning
a narrower set of variable ranges that satisfies the constraint.
-}
evaluate : EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range)
evaluate variables constraint =
  case constraint of
    Equal left right ->
      let
        allowed =
          Solver.Range.intersect (Solver.Term.evaluate variables left) (Solver.Term.evaluate variables right)
      in
        if Solver.Range.isEmpty allowed
        then Nothing
        else Just <| Solver.Term.constrain left allowed <| Solver.Term.constrain right allowed <| variables


{-| Get a list of bound variables in a constraint.
-}
boundVariables : Constraint var -> EveryDict var ()
boundVariables constraint =
  case constraint of
    Equal left right ->
      EveryDict.union (Solver.Term.boundVariables left) (Solver.Term.boundVariables right)
