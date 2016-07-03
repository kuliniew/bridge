module Solver.Constraint exposing
  ( Constraint
  , equal
  , lessThanOrEqual
  , lessThan
  , greaterThanOrEqual
  , greaterThan
  , and
  , all

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
  | LessThanOrEqual (Term var) (Term var)
  | And (Constraint var) (Constraint var)
  | AlwaysTrue


{-| Constrain two terms to be exactly equal.
-}
equal : Term var -> Term var -> Constraint var
equal =
  Equal


{-| Constrain one term to be less than or equal to another.
-}
lessThanOrEqual : Term var -> Term var -> Constraint var
lessThanOrEqual =
  LessThanOrEqual


{-| Constrain one term to be strictly less than another.
-}
lessThan : Term var -> Term var -> Constraint var
lessThan left right =
  lessThanOrEqual (left `Solver.Term.add` Solver.Term.constant 1) right


{-| Constrain one term to be greater than or equal to another.
-}
greaterThanOrEqual : Term var -> Term var -> Constraint var
greaterThanOrEqual =
  flip lessThanOrEqual


{-| Constrain one term to be strictly greater than another.
-}
greaterThan : Term var -> Term var -> Constraint var
greaterThan =
  flip lessThan


{-| Require both constraints to be met.
-}
and : Constraint var -> Constraint var -> Constraint var
and =
  And


{-| Require all constraints to be met.
-}
all : List (Constraint var) -> Constraint var
all =
  List.foldl and AlwaysTrue


{-| Evaluate a constraint over a set of known variable ranges, returning
a narrower set of variable ranges that satisfies the constraint.
-}
evaluate : EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range)
evaluate variables constraint =
  let
    basicResult =
      case constraint of
        Equal left right ->
          let
            allowed =
              Solver.Range.intersect (Solver.Term.evaluate variables left) (Solver.Term.evaluate variables right)
          in
            if Solver.Range.isEmpty allowed
            then Nothing
            else Just <| Solver.Term.constrain left allowed <| Solver.Term.constrain right allowed <| variables
        LessThanOrEqual left right ->
          let
            currentLeft =
              Solver.Term.evaluate variables left
            currentRight =
              Solver.Term.evaluate variables right
            allowedLeft =
              Solver.Range.intersect currentLeft (Solver.Range.removeLowerBound currentRight)
            allowedRight =
              Solver.Range.intersect currentRight (Solver.Range.removeUpperBound currentLeft)
          in
            if Solver.Range.isEmpty allowedLeft || Solver.Range.isEmpty allowedRight
            then Nothing
            else Just <| Solver.Term.constrain left allowedLeft <| Solver.Term.constrain right allowedRight <| variables
        And left right ->
          evaluate variables left `Maybe.andThen` flip evaluate right
        AlwaysTrue ->
          Just variables
  in
    case basicResult of
      Just newVariables ->
        if EveryDict.eq variables newVariables
        then basicResult
        else evaluate newVariables constraint
      Nothing ->
        basicResult


{-| Get a list of bound variables in a constraint.
-}
boundVariables : Constraint var -> EveryDict var ()
boundVariables constraint =
  case constraint of
    Equal left right ->
      EveryDict.union (Solver.Term.boundVariables left) (Solver.Term.boundVariables right)
    LessThanOrEqual left right ->
      EveryDict.union (Solver.Term.boundVariables left) (Solver.Term.boundVariables right)
    And left right ->
      EveryDict.union (boundVariables left) (boundVariables right)
    AlwaysTrue ->
      EveryDict.empty
