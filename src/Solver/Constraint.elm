module Solver.Constraint exposing
  ( Constraint
  , equal
  , lessThanOrEqual
  , lessThan
  , greaterThanOrEqual
  , greaterThan
  , and
  , all
  , or
  , any

  , evaluate
  , boundVariables

  , producer
  )

{-| Integer constraints over variables.
-}

import Solver.Range exposing (Range)
import Solver.Term exposing (Term)

import Check.Producer
import EveryDict exposing (EveryDict)
import Lazy.List
import Random
import Random.Extra
import Shrink


{-| A constraint to be satisfied.

Whenever a constraint implements a relation R, the interpretation is
"a R b".  For example, LessThan a b would be "a is less than b".
-}
type Constraint var
  = Equal (Term var) (Term var)
  | LessThanOrEqual (Term var) (Term var)
  | And (Constraint var) (Constraint var)
  | Or (Constraint var) (Constraint var)
  | AlwaysTrue
  | AlwaysFalse


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


{-| Require either constraint (or both) to be met.
-}
or : Constraint var -> Constraint var -> Constraint var
or =
  Or


{-| Require at least one constraint to be met.
-}
any : List (Constraint var) -> Constraint var
any =
  List.foldl or AlwaysFalse


{-| Evaluate a constraint over a set of known variable ranges, returning
a narrower set of variable ranges that satisfies the constraint.
-}
evaluate : EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range)
evaluate variables constraint =
  let
    sameVariablesOnBothSides left right =
      -- The solver can't currently handle things like "x = x + 1" gracefully since
      -- it really requires algebraic manipulation that isn't implemented yet.  So,
      -- give up if that case comes up, to avoid taking a loooooooong time to try to
      -- deal with it incrementally.
      not <| EveryDict.isEmpty <|
        EveryDict.intersect (Solver.Term.boundVariables left) (Solver.Term.boundVariables right)
    alternatives variables1 variables2 =
      EveryDict.keys variables1 ++ EveryDict.keys variables2
        |> List.map (\key -> (key, Solver.Range.union
                                      (Maybe.withDefault Solver.Range.full <| EveryDict.get key variables1)
                                      (Maybe.withDefault Solver.Range.full <| EveryDict.get key variables2)))
        |> EveryDict.fromList
    basicResult =
      case constraint of
        Equal left right ->
          let
            allowed =
              Solver.Range.intersect (Solver.Term.evaluate variables left) (Solver.Term.evaluate variables right)
          in
            if Solver.Range.isEmpty allowed || sameVariablesOnBothSides left right
            then Nothing
            else Solver.Term.constrain left allowed variables `Maybe.andThen` Solver.Term.constrain right allowed
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
            if Solver.Range.isEmpty allowedLeft || Solver.Range.isEmpty allowedRight || sameVariablesOnBothSides left right
            then Nothing
            else Solver.Term.constrain left allowedLeft variables `Maybe.andThen` Solver.Term.constrain right allowedRight
        And left right ->
          evaluate variables left `Maybe.andThen` flip evaluate right
        Or left right ->
          case (evaluate variables left, evaluate variables right) of
            (Just leftResult, Just rightResult) ->
              Just <| alternatives leftResult rightResult
            (Just leftResult, Nothing) ->
              Just leftResult
            (Nothing, Just rightResult) ->
              Just rightResult
            (Nothing, Nothing) ->
              Nothing
        AlwaysTrue ->
          Just variables
        AlwaysFalse ->
          Nothing
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
    Or left right ->
      EveryDict.union (boundVariables left) (boundVariables right)
    AlwaysTrue ->
      EveryDict.empty
    AlwaysFalse ->
      EveryDict.empty


{-| Produce a random constraint for testing.
-}
producer : Check.Producer.Producer var -> Check.Producer.Producer (Constraint var)
producer variableProducer =
  let
    termProducer =
      Solver.Term.producer variableProducer
  in
    { generator = constraintGenerator termProducer.generator
    , shrinker = constraintShrinker termProducer.shrinker
    }


-- All of these functions are at the toplevel because they are
-- mutually recursive generators/shrinkers.  If they were set in
-- a `let` binding, then in Elm 0.17 the compiler creates
-- JavaScript code that fails at runtime because of the circular
-- reference: something will be used before it's been initialized.


terminalGenerator : Random.Generator (Term var) -> Random.Generator (Constraint var)
terminalGenerator termGenerator =
  Random.Extra.choices
    [ Random.Extra.constant AlwaysTrue
    , Random.Extra.constant AlwaysFalse
    , Random.map2 Equal termGenerator termGenerator
    , Random.map2 LessThanOrEqual termGenerator termGenerator
    ]


nonTerminalGenerator : Random.Generator (Term var) -> Random.Generator (Constraint var)
nonTerminalGenerator termGenerator =
  let
    recursive =
      constraintGenerator termGenerator
  in
    Random.Extra.choices
      [ Random.map2 And recursive recursive
      , Random.map2 Or recursive recursive
      ]


constraintGenerator : Random.Generator (Term var) -> Random.Generator (Constraint var)
constraintGenerator termGenerator =
  -- The conditional here is essential, to prevent infinite recursion
  -- when evaluating the generator itself.
  Random.Extra.oneIn 4 `Random.andThen` \deeper ->
    if deeper
    then nonTerminalGenerator termGenerator
    else terminalGenerator termGenerator


constraintShrinker : Shrink.Shrinker (Term var) -> Shrink.Shrinker (Constraint var)
constraintShrinker termShrinker constraint =
  let
    shrinkTerms ctor left right =
      ctor `Shrink.map` termShrinker left `Shrink.andMap` termShrinker right
    shrinkConstraints ctor left right =
      Lazy.List.cons left <| Lazy.List.cons right <|
        ctor `Shrink.map` constraintShrinker termShrinker left `Shrink.andMap` constraintShrinker termShrinker right
  in
    case constraint of
      Equal left right ->
        shrinkTerms Equal left right
      LessThanOrEqual left right ->
        shrinkTerms LessThanOrEqual left right
      And left right ->
        shrinkConstraints And left right
      Or left right ->
        shrinkConstraints Or left right
      AlwaysTrue ->
        Lazy.List.empty
      AlwaysFalse ->
        Lazy.List.empty
