module Solver.TermTests exposing (all)

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


variablesProducer : Check.Producer.Producer (EveryDict String Range)
variablesProducer =
  let
    assignmentProducer =
      Check.Producer.tuple (Check.Producer.string, Solver.RangeTests.rangeProducer)
    assignmentsProducer =
      Check.Producer.list assignmentProducer
  in
    Check.Producer.convert EveryDict.fromList EveryDict.toList assignmentsProducer
