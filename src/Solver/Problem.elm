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
  case problem of
    Unsolvable ->
      Unsolvable
    Solvable state ->
      let
        bound =
          Solver.Constraint.boundVariables constraint
        save variable constraints =
          EveryDict.update variable add constraints
        add current =
          case current of
            Just constraints -> Just (constraint :: constraints)
            Nothing -> Just [constraint]
        newProblem =
          if EveryDict.size bound > 1
          then Solvable { state | constraints = List.foldl save state.constraints (EveryDict.keys bound) }
          else Solvable state
      in
        applyConstraint constraint newProblem


{-| Apply a constraint to the problem's current set of possible solutions,
and revisit any constraints involving the variables that changed.
-}
applyConstraint : Constraint var -> Problem var -> Problem var
applyConstraint constraint problem =
  case problem of
    Solvable state ->
      case Solver.Constraint.evaluate state.variables constraint of
        Just newVariables ->
          let
            changed =
              EveryDict.keys newVariables
                |> List.filter (\var -> EveryDict.get var newVariables /= EveryDict.get var state.variables)
            currentProblem =
              Solvable { state | variables = newVariables }
            rechecks =
              List.filter ((/=) constraint) (constraintsInvolving changed currentProblem)
          in
            List.foldl applyConstraint currentProblem rechecks
        Nothing ->
          Unsolvable
    Unsolvable ->
      Unsolvable


{-| Get all of the saved constraints that involve the affected variables,
without duplicates.
-}
constraintsInvolving : List var -> Problem var -> List (Constraint var)
constraintsInvolving variables problem =
  case problem of
    Unsolvable ->
      []
    Solvable state ->
      List.map (flip EveryDict.get state.constraints) variables
        |> List.map (Maybe.withDefault [])
        |> List.concat
        |> List.map (flip (,) ())
        |> EveryDict.fromList
        |> EveryDict.toList
        |> List.map fst
