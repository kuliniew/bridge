module Solver.Term exposing
  ( Term
  , constant
  , variable
  , add

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
  | Add (Term var) (Term var)


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


{-| The sum of two terms.
-}
add : Term var -> Term var -> Term var
add =
  Add


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
    Add left right ->
      evaluate variables left `Solver.Range.add` evaluate variables right


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
    Add left right ->
      let
        currentLeft =
          evaluate variables left
        currentRight =
          evaluate variables right
        allowedLeft =
          Solver.Range.intersect currentLeft (range `Solver.Range.subtract` currentRight)
        allowedRight =
          Solver.Range.intersect currentRight (range `Solver.Range.subtract` currentLeft)
      in
        constrain left allowedLeft <| constrain right allowedRight <| variables


{-| Get a list of bound variables in a term.
-}
boundVariables : Term var -> EveryDict var ()
boundVariables term =
  case term of
    Constant _ ->
      EveryDict.empty
    Variable variable ->
      EveryDict.singleton variable ()
    Add left right ->
      EveryDict.union (boundVariables left) (boundVariables right)
