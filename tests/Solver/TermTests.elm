module Solver.TermTests exposing (all)

import Solver.Endpoint
import Solver.Interval exposing (Interval)
import Solver.Range exposing (Range)
import Solver.RangeTests exposing (rangeProducer)
import Solver.Term
import TestUtils

import Check
import Check.Producer
import ElmTest
import EveryDict exposing (EveryDict)
import List.Extra


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
          fst
        `Check.for`
          Check.Producer.tuple (variablesProducer, Check.Producer.int)

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
          (\(variable, range) -> EveryDict.singleton variable range)
        `Check.for`
          (Check.Producer.tuple (Check.Producer.string, rangeProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "can be constrained to a subset of its original value"
        `Check.that`
          (\(variable, range, subset) -> Solver.Term.constrain (Solver.Term.variable variable) subset (EveryDict.singleton variable range))
        `Check.is`
          (\(variable, _, subset) -> EveryDict.singleton variable subset)
        `Check.for`
          Check.Producer.filter
            (\(_, _, subset) -> not <| Solver.Range.isEmpty subset)
            (Check.Producer.map
              (\(variable, range, mask) -> (variable, range, Solver.Range.intersect range mask))
              (Check.Producer.tuple3 (Check.Producer.string, rangeProducer, rangeProducer)))

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
          always EveryDict.empty
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
          (\(variable, addend, sum) -> EveryDict.singleton variable (Solver.Range.singleton (sum - addend)))
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
          (\(variable, addend, sum) -> EveryDict.singleton variable (Solver.Range.singleton (sum - addend)))
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
            EveryDict.fromList
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
            EveryDict.fromList
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


variablesProducer : Check.Producer.Producer (EveryDict String Range)
variablesProducer =
  let
    assignmentProducer =
      Check.Producer.tuple (Check.Producer.string, Solver.RangeTests.rangeProducer)
    assignmentsProducer =
      Check.Producer.list assignmentProducer
  in
    Check.Producer.convert EveryDict.fromList EveryDict.toList assignmentsProducer


boundedInterval : Int -> Int -> Interval
boundedInterval lo hi =
  case Solver.Interval.fromEndpoints (Solver.Endpoint.Point lo) (Solver.Endpoint.Point hi) of
    Just interval ->
      interval
    Nothing ->
      Debug.crash <| "boundedInterval failed for: " ++ toString lo ++ " " ++ toString hi
