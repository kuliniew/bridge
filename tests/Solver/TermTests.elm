module Solver.TermTests exposing (all)

import Solver.Endpoint
import Solver.Interval exposing (Interval)
import Solver.Range exposing (Range)
import Solver.RangeTests exposing (nonEmptyRangeProducer, rangeProducer)
import Solver.Term exposing (Term)
import TestUtils

import Check
import Check.Producer
import ElmTest
import EveryDict exposing (EveryDict)
import List.Extra
import Random
import Random.Extra
import Shrink


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Term"
    [ constantSuite
    , variableSuite
    , addSuite
    , sumSuite
    ]


constantSuite : ElmTest.Test
constantSuite =
  ElmTest.suite "constant"
    [ TestUtils.generativeTest <|
        Check.claim
          "always evaluates to the same value"
        `Check.that`
          (\(variables, value) -> Solver.Term.evaluate variables (Solver.Term.constant value))
        `Check.is`
          (\(_, value) -> Solver.Range.singleton value)
        `Check.for`
          Check.Producer.tuple (variablesProducer, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "can be constrained to that value"
        `Check.that`
          (\(variables, value) -> Solver.Term.constrain (Solver.Term.constant value) (Solver.Range.singleton value) variables)
        `Check.is`
          (Just << fst)
        `Check.for`
          Check.Producer.tuple (variablesProducer, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "can be constrained to ranges including that value"
        `Check.that`
          (\(variables, value, range) -> Solver.Term.constrain (Solver.Term.constant value) range variables)
        `Check.is`
          (\(variables, _, _) -> Just variables)
        `Check.for`
          Check.Producer.filter
            (\(_, value, range) -> Solver.Range.member value range)
            (Check.Producer.tuple3 (variablesProducer, Check.Producer.int, rangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "cannot be constrained to ranges not including that value"
        `Check.that`
          (\(variables, value, range) -> Solver.Term.constrain (Solver.Term.constant value) range variables)
        `Check.is`
          always Nothing
        `Check.for`
          Check.Producer.filter
            (\(_, value, range) -> not <| Solver.Range.member value range)
            (Check.Producer.tuple3 (variablesProducer, Check.Producer.int, rangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "has no bound variables"
        `Check.that`
          (EveryDict.keys << Solver.Term.boundVariables << Solver.Term.constant)
        `Check.is`
          always []
        `Check.for`
          Check.Producer.int
    ]


variableSuite : ElmTest.Test
variableSuite =
  ElmTest.suite "variable"
    [ TestUtils.generativeTest <|
        Check.claim
          "evaluates to what's in the problem state"
        `Check.that`
          (\(variable, range) -> Solver.Term.evaluate (EveryDict.singleton variable range) (Solver.Term.variable variable))
        `Check.is`
          snd
        `Check.for`
          (Check.Producer.tuple (Check.Producer.string, rangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "evaluates to everything if not in the problem state"
        `Check.that`
          (\variable -> Solver.Term.evaluate EveryDict.empty (Solver.Term.variable variable))
        `Check.is`
          always Solver.Range.full
        `Check.for`
          Check.Producer.string

    , TestUtils.generativeTest <|
        Check.claim
          "can be constrained to its original value"
        `Check.that`
          (\(variable, range) -> Solver.Term.constrain (Solver.Term.variable variable) range (EveryDict.singleton variable range))
        `Check.is`
          (\(variable, range) -> Just <| EveryDict.singleton variable range)
        `Check.for`
          (Check.Producer.tuple (Check.Producer.string, nonEmptyRangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "can be constrained to a subset of its original value"
        `Check.that`
          (\(variable, range, otherRange) -> Solver.Term.constrain (Solver.Term.variable variable) otherRange (EveryDict.singleton variable range))
        `Check.is`
          (\(variable, range, otherRange) -> Just <| EveryDict.singleton variable (Solver.Range.intersect range otherRange))
        `Check.for`
          Check.Producer.filter
            (\(_, range, otherRange) -> not <| Solver.Range.isEmpty <| Solver.Range.intersect range otherRange)
            (Check.Producer.tuple3 (Check.Producer.string, rangeProducer, rangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "cannot be constrained to a range that does not overlap its current value"
        `Check.that`
          (\(variable, range, otherRange) -> Solver.Term.constrain (Solver.Term.variable variable) otherRange (EveryDict.singleton variable range))
        `Check.is`
          always Nothing
        `Check.for`
          Check.Producer.filter
            (\(_, range, otherRange) -> Solver.Range.isEmpty <| Solver.Range.intersect range otherRange)
            (Check.Producer.tuple3 (Check.Producer.string, rangeProducer, rangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "has itself as a bound variable"
        `Check.that`
          (EveryDict.keys << Solver.Term.boundVariables << Solver.Term.variable)
        `Check.is`
          List.Extra.singleton
        `Check.for`
          Check.Producer.string
    ]


addSuite : ElmTest.Test
addSuite =
  ElmTest.suite "add"
    [ TestUtils.generativeTest <|
        Check.claim
          "always evaluates to the sum of its subterms"
        `Check.that`
          (\(variables, value1, value2) -> Solver.Term.evaluate variables (Solver.Term.constant value1 `Solver.Term.add` Solver.Term.constant value2))
        `Check.is`
          (\(_, value1, value2) -> Solver.Range.singleton (value1 + value2))
        `Check.for`
          Check.Producer.tuple3 (variablesProducer, Check.Producer.int, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "can constrain a sum of constants to their actual values"
        `Check.that`
          (\(value1, value2) ->
            Solver.Term.constrain
              (Solver.Term.constant value1 `Solver.Term.add` Solver.Term.constant value2)
              (Solver.Range.singleton (value1 + value2))
              EveryDict.empty
          )
        `Check.is`
          always (Just <| EveryDict.empty)
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "can constrain a variable added to a constant"
        `Check.that`
          (\(variable, addend, sum) ->
            Solver.Term.constrain
              (Solver.Term.variable variable `Solver.Term.add` Solver.Term.constant addend)
              (Solver.Range.singleton sum)
              EveryDict.empty
          )
        `Check.is`
          (\(variable, addend, sum) -> Just <| EveryDict.singleton variable (Solver.Range.singleton (sum - addend)))
        `Check.for`
          Check.Producer.tuple3 (Check.Producer.string, Check.Producer.int, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "can constrain a constant added to a variable"
        `Check.that`
          (\(variable, addend, sum) ->
            Solver.Term.constrain
              (Solver.Term.constant addend `Solver.Term.add` Solver.Term.variable variable)
              (Solver.Range.singleton sum)
              EveryDict.empty
          )
        `Check.is`
          (\(variable, addend, sum) -> Just <| EveryDict.singleton variable (Solver.Range.singleton (sum - addend)))
        `Check.for`
          Check.Producer.tuple3 (Check.Producer.string, Check.Producer.int, Check.Producer.int)

    , ElmTest.test "x:[1 .. 3] + y:[4 .. 6] == [5 .. 5]" <|
        let
          variables =
            EveryDict.fromList
              [ ("x", Solver.Range.fromIntervals [boundedInterval 1 3])
              , ("y", Solver.Range.fromIntervals [boundedInterval 4 6])
              ]
          constraint =
            Solver.Term.variable "x" `Solver.Term.add` Solver.Term.variable "y"
          expected =
            Just <| EveryDict.fromList
              [ ("x", Solver.Range.singleton 1)
              , ("y", Solver.Range.singleton 4)
              ]
        in
          ElmTest.assertEqual expected (Solver.Term.constrain constraint (Solver.Range.singleton 5) variables)

    , ElmTest.test "x:[1 .. 3] + y:[4 .. 6] == [6 .. 6]" <|
        let
          variables =
            EveryDict.fromList
              [ ("x", Solver.Range.fromIntervals [boundedInterval 1 3])
              , ("y", Solver.Range.fromIntervals [boundedInterval 4 6])
              ]
          constraint =
            Solver.Term.variable "x" `Solver.Term.add` Solver.Term.variable "y"
          expected =
            Just <| EveryDict.fromList
              [ ("x", Solver.Range.fromIntervals [boundedInterval 1 2])
              , ("y", Solver.Range.fromIntervals [boundedInterval 4 5])
              ]
        in
          ElmTest.assertEqual expected (Solver.Term.constrain constraint (Solver.Range.singleton 6) variables)

    , TestUtils.generativeTest <|
        Check.claim
          "has the bound variables of its subterms"
        `Check.that`
          (\(variable1, variable2) ->
            EveryDict.toList <| Solver.Term.boundVariables (Solver.Term.variable variable1 `Solver.Term.add` Solver.Term.variable variable2))
        `Check.is`
          (\(variable1, variable2) -> EveryDict.toList <| EveryDict.fromList [(variable1, ()), (variable2, ())])
        `Check.for`
          Check.Producer.tuple (Check.Producer.string, Check.Producer.string)

    , commutativeTests Solver.Term.add

    , associativeTests Solver.Term.add

    , leftIdentityTests Solver.Term.add (Solver.Term.constant 0)
    ]


sumSuite : ElmTest.Test
sumSuite =
  ElmTest.suite "sum"
    [ ElmTest.test "for zero terms, evaluates to zero" <|
        ElmTest.assertEqual
          (Solver.Range.singleton 0)
          (Solver.Term.evaluate EveryDict.empty <| Solver.Term.sum [])

    , TestUtils.generativeTest <|
        Check.claim
          "for one term, evaluates to that term"
        `Check.that`
          (\value -> Solver.Term.evaluate EveryDict.empty <| Solver.Term.sum [Solver.Term.constant value])
        `Check.is`
          Solver.Range.singleton
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "for two terms, is consistent with add"
        `Check.that`
          (\(value1, value2) -> Solver.Term.evaluate EveryDict.empty <| Solver.Term.sum (List.map Solver.Term.constant [value1, value2]))
        `Check.is`
          (\(value1, value2) -> Solver.Term.evaluate EveryDict.empty <| Solver.Term.add (Solver.Term.constant value1) (Solver.Term.constant value2))
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "sums lists of arbitrary length correctly"
        `Check.that`
          (\values -> Solver.Term.evaluate EveryDict.empty <| Solver.Term.sum (List.map Solver.Term.constant values))
        `Check.is`
          (Solver.Range.singleton << List.sum)
        `Check.for`
          Check.Producer.list Check.Producer.int
    ]


commutativeTests : (Term String -> Term String -> Term String) -> ElmTest.Test
commutativeTests operation =
  ElmTest.suite "commutative"
    [ TestUtils.generativeTest <|
        Check.claim
          "evaluate"
        `Check.that`
          (\(term1, term2, variables) -> Solver.Term.evaluate variables (term1 `operation` term2))
        `Check.is`
          (\(term1, term2, variables) -> Solver.Term.evaluate variables (term2 `operation` term1))
        `Check.for`
          Check.Producer.tuple3 (termProducer, termProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "constrain"
        `Check.that`
          (\(term1, term2, range, variables) -> constrainToList (term1 `operation` term2) range variables)
        `Check.is`
          (\(term1, term2, range, variables) -> constrainToList (term2 `operation` term1) range variables)
        `Check.for`
          Check.Producer.tuple4 (termProducer, termProducer, rangeProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "boundVariables"
        `Check.that`
          (\(term1, term2) -> boundVariablesToList (term1 `operation` term2))
        `Check.is`
          (\(term1, term2) -> boundVariablesToList (term2 `operation` term1))
        `Check.for`
          Check.Producer.tuple (termProducer, termProducer)
    ]


associativeTests : (Term String -> Term String -> Term String) -> ElmTest.Test
associativeTests operation =
  ElmTest.suite "associative"
    [ TestUtils.generativeTest <|
        Check.claim
          "evaluate"
        `Check.that`
          (\(term1, term2, term3, variables) -> Solver.Term.evaluate variables (term1 `operation` (term2 `operation` term3)))
        `Check.is`
          (\(term1, term2, term3, variables) -> Solver.Term.evaluate variables ((term1 `operation` term2) `operation` term3))
        `Check.for`
          Check.Producer.tuple4 (termProducer, termProducer, termProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "constrain"
        `Check.that`
          (\(term1, term2, term3, range, variables) -> constrainToList (term1 `operation` (term2 `operation` term3)) range variables)
        `Check.is`
          (\(term1, term2, term3, range, variables) -> constrainToList ((term1 `operation` term2) `operation` term3) range variables)
        `Check.for`
          Check.Producer.tuple5 (termProducer, termProducer, termProducer, rangeProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "boundVariables"
        `Check.that`
          (\(term1, term2, term3) -> boundVariablesToList (term1 `operation` (term2 `operation` term3)))
        `Check.is`
          (\(term1, term2, term3) -> boundVariablesToList ((term1 `operation` term2) `operation` term3))
        `Check.for`
          Check.Producer.tuple3 (termProducer, termProducer, termProducer)
    ]


leftIdentityTests : (Term String -> Term String -> Term String) -> Term String -> ElmTest.Test
leftIdentityTests operation leftIdentity =
  ElmTest.suite "left identity"
    [ TestUtils.generativeTest <|
        Check.claim
          "evaluate"
        `Check.that`
          (\(term, variables) -> Solver.Term.evaluate variables (leftIdentity `operation` term))
        `Check.is`
          (\(term, variables) -> Solver.Term.evaluate variables (leftIdentity `operation` term))
        `Check.for`
          Check.Producer.tuple (termProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "constrain"
        `Check.that`
          (\(term, range, variables) -> constrainToList (leftIdentity `operation` term) range variables)
        `Check.is`
          (\(term, range, variables) -> constrainToList (leftIdentity `operation` term) range variables)
        `Check.for`
          Check.Producer.tuple3 (termProducer, rangeProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "boundVariables"
        `Check.that`
          (\term -> boundVariablesToList (leftIdentity `operation` term))
        `Check.is`
          (\term -> boundVariablesToList (leftIdentity `operation` term))
        `Check.for`
          termProducer
    ]


variableNames : List String
variableNames =
  ["w", "x", "y", "z"]


variableProducer : Check.Producer.Producer String
variableProducer =
  { generator =
      Random.Extra.choices <| List.map Random.Extra.constant variableNames
  , shrinker =
      Shrink.noShrink
  }


variablesProducer : Check.Producer.Producer (EveryDict String Range)
variablesProducer =
  { generator =
      List.repeat (List.length variableNames) nonEmptyRangeProducer.generator
        |> Random.Extra.together
        |> Random.map (List.Extra.zip variableNames)
        |> Random.map EveryDict.fromList
  , shrinker =
      Shrink.noShrink
  }


termProducer : Check.Producer.Producer (Term String)
termProducer =
  Solver.Term.producer variableProducer


boundedInterval : Int -> Int -> Interval
boundedInterval lo hi =
  case Solver.Interval.fromEndpoints (Solver.Endpoint.Point lo) (Solver.Endpoint.Point hi) of
    Just interval ->
      interval
    Nothing ->
      Debug.crash <| "boundedInterval failed for: " ++ toString lo ++ " " ++ toString hi


constrainToList : Term var -> Range -> EveryDict var Range -> Maybe (List (var, Range))
constrainToList term range variables =
  Maybe.map EveryDict.toList <| Solver.Term.constrain term range variables


boundVariablesToList : Term var -> List var
boundVariablesToList term =
  EveryDict.keys <| Solver.Term.boundVariables term
