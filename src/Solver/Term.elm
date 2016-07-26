module Solver.Term exposing
  ( Term
  , constant
  , variable
  , add
  , sum
  , negate

  , eq

  , evaluate
  , constrain
  , boundVariables

  , producer
  )

{-| Individual term found in constraints.
-}

import Solver.Range exposing (Range)

import Check.Producer
import EveryDict exposing (EveryDict)
import Random
import Random.Extra
import Shrink


{-| A term in a constraint.

Internally, a term is a sum of variables and their coefficients,
plus a constant.  This representation facilitates basic algebraic
manipulations.
-}
type Term var
  = Term (EveryDict (Maybe var) Int)


{-| A constant value.
-}
constant : Int -> Term var
constant const =
  Term <|
    if const == 0
    then EveryDict.empty
    else EveryDict.singleton Nothing const


{-| A variable.
-}
variable : var -> Term var
variable var =
  Term <| EveryDict.singleton (Just var) 1


{-| The sum of two terms.
-}
add : Term var -> Term var -> Term var
add (Term term1) (Term term2) =
  let
    doUpdate coeffDelta current =
      case coeffDelta + Maybe.withDefault 0 current of
        0 ->
          Nothing
        newCoeff ->
          Just newCoeff
    include var coeff =
      EveryDict.update var (doUpdate coeff)
  in
    Term <| EveryDict.foldl include term1 term2


{-| The sum of an arbitrary number of terms.
-}
sum : List (Term var) -> Term var
sum =
  List.foldl add (constant 0)


{-| The negation of a term.
-}
negate : Term var -> Term var
negate (Term term) =
  Term <| EveryDict.map (always Basics.negate) term


{-| Check if two terms are equivalent.
-}
eq : Term var -> Term var -> Bool
eq (Term left) (Term right) =
  EveryDict.eq left right


{-| Evaluate the range of values a term can have, given a set of known
variable ranges.
-}
evaluate : EveryDict var Range -> Term var -> Range
evaluate variables (Term term) =
  evaluate' variables (EveryDict.toList term)


{-| Evaluate the range of values the parts of a term can have, given
a set of known variable ranges.
-}
evaluate' : EveryDict var Range -> List (Maybe var, Int) -> Range
evaluate' variables parts =
  let
    evaluateVar var =
      case var of
        Just variable ->
          Maybe.withDefault Solver.Range.full <| EveryDict.get variable variables
        Nothing ->
          Solver.Range.singleton 1
  in
    Solver.Range.sum <| List.map (\(var, coeff) -> Solver.Range.multiply coeff (evaluateVar var)) parts


{-| Constrain the ranges of variables based on the known range of a
term.
-}
constrain : Term var -> Range -> EveryDict var Range -> Maybe (EveryDict var Range)
constrain (Term term) range variables =
  constrain' (EveryDict.toList term) range variables


{-| Constrain the ranges of variables based on the known range of
parts of a term.
-}
constrain' : List (Maybe var, Int) -> Range -> EveryDict var Range -> Maybe (EveryDict var Range)
constrain' parts range variables =
  let
    constrainVar var coeff allowedProduct =
      case var of
        Just variable ->
          let
            oldRange =
              Maybe.withDefault Solver.Range.full <| EveryDict.get variable variables
            newRange =
              Solver.Range.intersect oldRange (allowedProduct `Solver.Range.divide` coeff)
          in
            if Solver.Range.isEmpty newRange
            then Nothing
            else Just <| EveryDict.insert variable newRange variables
        Nothing ->
          if Solver.Range.member coeff allowedProduct
          then Just variables
          else Nothing
  in
    case parts of
      [] ->
        if Solver.Range.member 0 range
        then Just variables
        else Nothing
      (var, coeff) :: rest ->
        let
          currentVar =
            evaluate' variables [(var, coeff)]
          currentRest =
            evaluate' variables rest
          allowedVar =
            Solver.Range.intersect currentVar (range `Solver.Range.subtract` currentRest)
          allowedRest =
            Solver.Range.intersect currentRest (range `Solver.Range.subtract` currentVar)
        in
          constrainVar var coeff allowedVar `Maybe.andThen` constrain' rest allowedRest


{-| Get a list of bound variables in a term.
-}
boundVariables : Term var -> EveryDict var ()
boundVariables (Term term) =
  EveryDict.keys term
    |> List.filterMap identity
    |> List.map (\var -> (var, ()))
    |> EveryDict.fromList


{-| Produce a random term for testing.
-}
producer : Check.Producer.Producer var -> Check.Producer.Producer (Term var)
producer variableProducer =
  { generator = termGenerator variableProducer.generator
  , shrinker = Shrink.noShrink
  }


termGenerator : Random.Generator var -> Random.Generator (Term var)
termGenerator varGenerator =
  Random.Extra.oneIn 4 `Random.andThen` \deeper ->
    if deeper
    then nonTerminalTermGenerator varGenerator
    else terminalTermGenerator varGenerator


nonTerminalTermGenerator : Random.Generator var -> Random.Generator (Term var)
nonTerminalTermGenerator varGenerator =
  let
    recursive =
      termGenerator varGenerator
  in
    Random.Extra.choices
      [ Random.map2 add recursive recursive
      , Random.map negate recursive
      ]


terminalTermGenerator : Random.Generator var -> Random.Generator (Term var)
terminalTermGenerator varGenerator =
  Random.Extra.choices
    [ Random.map constant (Random.int -100000 100000)
    , Random.map variable varGenerator
    ]
