module Solver.Problem exposing
  ( Problem
  , empty
  , isSolvable
  , possibleValues
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
  = Problem
      { variables : EveryDict var Range
      , constraints : EveryDict var (List (Constraint var))
      }


{-| A constraint problem with no constraints.
-}
empty : Problem var
empty =
  Problem
    { variables = EveryDict.empty
    , constraints = EveryDict.empty
    }


{-| Check if the problem is still solvable.
-}
isSolvable : Problem var -> Bool
isSolvable (Problem problem) =
  List.all (not << Solver.Range.isEmpty) (EveryDict.values problem.variables)


{-| Get the range of possible values for a variable.
-}
possibleValues : var -> Problem var -> Range
possibleValues variable (Problem problem) =
  Maybe.withDefault Solver.Range.full <| EveryDict.get variable problem.variables
