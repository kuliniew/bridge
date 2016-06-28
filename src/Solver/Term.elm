module Solver.Term exposing
  ( Term
  , constant
  , variable

  , evaluate
  , constrain
  , boundVariables
  )

{-| Individual terms found in constraints.
-}

import Solver.Range exposing (Range)

import EveryDict exposing (EveryDict)


{-| A term in a constraint.
-}
type Term var
  = Constant Int
  | Variable var


{-| A constant value.
-}
constant : Int -> Term var
constant =
  Constant


{-| A variable.
-}
variable : var -> Term var
variable =
  Variable


{-| Evaluate the range of vaues a term can have, given a set of known
variable ranges.
-}
evaluate : EveryDict var Range -> Term var -> Range
evaluate variables term =
  case term of
    Constant value ->
      Solver.Range.singleton value
    Variable variable ->
      Maybe.withDefault Solver.Range.full <| EveryDict.get variable variables


{-| Constrain the ranges of variables based on the known range of a
term.
-}
constrain : Term var -> Range -> EveryDict var Range -> EveryDict var Range
constrain term range variables =
  case term of
    Constant value ->
      if Solver.Range.member value range
      then variables
      else Debug.crash <| "tried to constrain " ++ toString term ++ " to range " ++ toString range
    Variable variable ->
      let
        oldRange =
          evaluate variables term
      in
        if Solver.Range.subset range oldRange
        then EveryDict.insert variable range variables
        else Debug.crash <| "tried to constrain " ++ toString term ++ " from range " ++ toString oldRange ++ " to range " ++ toString range


{-| Get a list of bound variables in a term.
-}
boundVariables : Term var -> EveryDict var ()
boundVariables term =
  case term of
    Constant _ ->
      EveryDict.empty
    Variable variable ->
      EveryDict.singleton variable ()
