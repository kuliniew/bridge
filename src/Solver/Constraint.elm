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
  , not
  , ifThen
  , ifThenElse

  , decompose
  , evaluate
  , boundVariables

  , mapVariables
  , eq

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
  = Zero (Term var)
  | Positive (Term var)
  | And (Constraint var) (Constraint var)
  | Or (Constraint var) (Constraint var)
  | AlwaysTrue
  | AlwaysFalse


{-| Constrain two terms to be exactly equal.
-}
equal : Term var -> Term var -> Constraint var
equal left right =
  Zero (left `Solver.Term.subtract` right)


{-| Constrain one term to be less than or equal to another.
-}
lessThanOrEqual : Term var -> Term var -> Constraint var
lessThanOrEqual left right =
  lessThan left (right `Solver.Term.add` Solver.Term.constant 1)


{-| Constrain one term to be strictly less than another.
-}
lessThan : Term var -> Term var -> Constraint var
lessThan left right =
  Positive (right `Solver.Term.subtract` left)


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


{-| Require that a constraint not be met.
-}
not : Constraint var -> Constraint var
not constraint =
  case constraint of
    Zero term ->
      (term `lessThan` Solver.Term.constant 0) `or` (term `greaterThan` Solver.Term.constant 0)
    Positive term ->
      (term `lessThanOrEqual` Solver.Term.constant 0)
    And left right ->
      not left `or` not right
    Or left right ->
      not left `and` not right
    AlwaysTrue ->
      AlwaysFalse
    AlwaysFalse ->
      AlwaysTrue


{-| If one constraint is true, require a second constraint to
also be true.
-}
ifThen : Constraint var -> Constraint var -> Constraint var
ifThen cond whenTrue =
  not cond `or` whenTrue


{-| If one constraint is true, require a second constraint to
also be true.  Otherwise, require a third constraint to be true
instead.
-}
ifThenElse : Constraint var -> Constraint var -> Constraint var -> Constraint var
ifThenElse cond whenTrue whenFalse =
  all
    [ ifThen cond whenTrue
    , ifThen (not cond) whenFalse
    , whenTrue `or` whenFalse
    ]


{-| Decompose a constraint into a list of smaller, equivalent constraints.
The solver can be more efficient with a larger number of smaller constraints,
since it can consider fewer variables at a time.
-}
decompose : Constraint var -> List (Constraint var)
decompose constraint =
  case constraint of
    And left right ->
      decompose left ++ decompose right
    _ ->
      [constraint]


{-| Evaluate a constraint over a set of known variable ranges, returning
a narrower set of variable ranges that satisfies the constraint.
-}
evaluate : EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range)
evaluate variables constraint =
  let
    alternatives variables1 variables2 =
      EveryDict.keys variables1 ++ EveryDict.keys variables2
        |> List.map (\key -> (key, Solver.Range.union
                                      (Maybe.withDefault Solver.Range.full <| EveryDict.get key variables1)
                                      (Maybe.withDefault Solver.Range.full <| EveryDict.get key variables2)))
        |> EveryDict.fromList
    basicResult =
      case constraint of
        Zero term ->
          Solver.Term.constrain term (Solver.Range.singleton 0) variables
        Positive term ->
          Solver.Term.constrain term (Solver.Range.fromLowerBound 1) variables
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
    Zero term ->
      Solver.Term.boundVariables term
    Positive term ->
      Solver.Term.boundVariables term
    And left right ->
      EveryDict.union (boundVariables left) (boundVariables right)
    Or left right ->
      EveryDict.union (boundVariables left) (boundVariables right)
    AlwaysTrue ->
      EveryDict.empty
    AlwaysFalse ->
      EveryDict.empty


{-| Apply a transformation to the variables in a constraint.
-}
mapVariables : (old -> new) -> Constraint old -> Constraint new
mapVariables f constraint =
  case constraint of
    Zero child ->
      Zero (Solver.Term.mapVariables f child)
    Positive child ->
      Positive (Solver.Term.mapVariables f child)
    And child1 child2 ->
      And (mapVariables f child1) (mapVariables f child2)
    Or child1 child2 ->
      Or (mapVariables f child1) (mapVariables f child2)
    AlwaysTrue ->
      AlwaysTrue
    AlwaysFalse ->
      AlwaysFalse


{-| Test if two constraints are exactly identical.
-}
eq : Constraint var -> Constraint var -> Bool
eq left right =
  case (left, right) of
    (Zero leftChild, Zero rightChild) ->
      Solver.Term.eq leftChild rightChild
    (Positive leftChild, Positive rightChild) ->
      Solver.Term.eq leftChild rightChild
    (And leftChild1 leftChild2, And rightChild1 rightChild2) ->
      eq leftChild1 rightChild1 && eq leftChild2 rightChild2
    (Or leftChild1 leftChild2, Or rightChild1 rightChild2) ->
      eq leftChild1 rightChild1 && eq leftChild2 rightChild2
    (AlwaysTrue, AlwaysTrue) ->
      True
    (AlwaysFalse, AlwaysFalse) ->
      True
    _ ->
      False


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
    , Random.map Zero termGenerator
    , Random.map Positive termGenerator
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
    shrinkConstraints ctor left right =
      Lazy.List.cons left <| Lazy.List.cons right <|
        ctor `Shrink.map` constraintShrinker termShrinker left `Shrink.andMap` constraintShrinker termShrinker right
  in
    case constraint of
      Zero term ->
        Zero `Shrink.map` termShrinker term
      Positive term ->
        Positive `Shrink.map` termShrinker term
      And left right ->
        shrinkConstraints And left right
      Or left right ->
        shrinkConstraints Or left right
      AlwaysTrue ->
        Lazy.List.empty
      AlwaysFalse ->
        Lazy.List.empty
