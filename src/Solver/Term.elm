module Solver.Term exposing
  ( Term
  , constant
  , variable
  , add
  , sum

  , evaluate
  , constrain
  , boundVariables

  , producer
  )

{-| Individual terms found in constraints.
-}

import Solver.Range exposing (Range)

import Check.Producer
import EveryDict exposing (EveryDict)
import Lazy.List
import Random
import Random.Extra
import Shrink


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


{-| The sum of an arbitrary number of terms.
-}
sum : List (Term var) -> Term var
sum =
  List.foldl add (constant 0)


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
constrain : Term var -> Range -> EveryDict var Range -> Maybe (EveryDict var Range)
constrain term range variables =
  case term of
    Constant value ->
      if Solver.Range.member value range
      then Just variables
      else Nothing
    Variable variable ->
      let
        oldRange =
          evaluate variables term
        allowed =
          Solver.Range.intersect range oldRange
      in
        if Solver.Range.isEmpty allowed
        then Nothing
        else Just <| EveryDict.insert variable allowed variables
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
        constrain right allowedRight variables `Maybe.andThen` constrain left allowedLeft


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


{-| Produce a random term for testing.
-}
producer : Check.Producer.Producer var -> Check.Producer.Producer (Term var)
producer variableProducer =
  { generator = termGenerator variableProducer.generator
  , shrinker = termShrinker variableProducer.shrinker
  }


constantGenerator : Random.Generator (Term var)
constantGenerator =
  let
    intProducer = Check.Producer.int
  in
    Random.map Constant intProducer.generator


variableGenerator : Random.Generator var -> Random.Generator (Term var)
variableGenerator varGenerator =
  Random.map Variable varGenerator


addGenerator : Random.Generator var -> Random.Generator (Term var)
addGenerator varGenerator =
  Random.map2 Add (terminalGenerator varGenerator) (terminalGenerator varGenerator)


terminalGenerator : Random.Generator var -> Random.Generator (Term var)
terminalGenerator varGenerator =
  Random.Extra.choices [constantGenerator, variableGenerator varGenerator]


nonTerminalGenerator : Random.Generator var -> Random.Generator (Term var)
nonTerminalGenerator varGenerator =
  addGenerator varGenerator


termGenerator : Random.Generator var -> Random.Generator (Term var)
termGenerator varGenerator =
  Random.Extra.frequency
    [ (0.7, terminalGenerator varGenerator)
    , (0.3, nonTerminalGenerator varGenerator)
    ]


constantShrinker : Int -> Lazy.List.LazyList (Term var)
constantShrinker value =
  Constant `Shrink.map` Shrink.int value


variableShrinker : Shrink.Shrinker var -> var -> Lazy.List.LazyList (Term var)
variableShrinker varShrinker var =
  Variable `Shrink.map` varShrinker var


addShrinker : Shrink.Shrinker var -> Term var -> Term var -> Lazy.List.LazyList (Term var)
addShrinker varShrinker left right =
  Lazy.List.cons left <| Lazy.List.cons right <| Add `Shrink.map` termShrinker varShrinker left `Shrink.andMap` termShrinker varShrinker right


termShrinker : Shrink.Shrinker var ->  Shrink.Shrinker (Term var)
termShrinker varShrinker term =
  case term of
    Constant value ->
      constantShrinker value
    Variable var ->
      variableShrinker varShrinker var
    Add left right ->
      addShrinker varShrinker left right
