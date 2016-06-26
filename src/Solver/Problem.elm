module Solver.Problem exposing
  ( Problem
  , empty
  , isSolvable
  , possibleValues
  , addConstraint
  )

{-| A constraint problem being solved.
-}

import Solver.Constraint exposing (Constraint)
import Solver.Range exposing (Range)

import EveryDict exposing (EveryDict)


{-| A constraint problem over integers.  Operations on a problem will only
restrict the set of possible solutions; no operations will introduce new
possible solutions.
-}
type Problem var
  = Solvable
      { variables : EveryDict var Range
      , constraints : EveryDict var (List (Constraint var))
      }
  | Unsolvable


{-| A constraint problem with no constraints.
-}
empty : Problem var
empty =
  Solvable
    { variables = EveryDict.empty
    , constraints = EveryDict.empty
    }


{-| Check if the problem is still solvable.
-}
isSolvable : Problem var -> Bool
isSolvable problem =
  case problem of
    Solvable state ->
      -- is this necessary?
      List.all (not << Solver.Range.isEmpty) (EveryDict.values state.variables)
    Unsolvable ->
      False


{-| Get the range of possible values for a variable.
-}
possibleValues : var -> Problem var -> Range
possibleValues variable problem =
  case problem of
    Solvable state ->
      Maybe.withDefault Solver.Range.full <| EveryDict.get variable state.variables
    Unsolvable ->
      Solver.Range.empty


{-| Add a new constraint to the problem.
-}
addConstraint : Constraint var -> Problem var -> Problem var
addConstraint constraint problem =
  applyConstraint constraint problem


{-| Apply a constraint to the problem's current set of possible solutions.
-}
applyConstraint : Constraint var -> Problem var -> Problem var
applyConstraint constraint problem =
  case problem of
    Solvable state ->
      case Solver.Constraint.evaluate state.variables constraint of
        Just newVariables ->
          Solvable { state | variables = newVariables }
        Nothing ->
          Unsolvable
    Unsolvable ->
      Unsolvable
