module Solver.TermTests exposing (all)

import Solver.Range exposing (Range)
import Solver.RangeTests
import Solver.Term
import TestUtils

import Check
import Check.Producer
import ElmTest
import EveryDict exposing (EveryDict)


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Term"
    [ constantSuite
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
