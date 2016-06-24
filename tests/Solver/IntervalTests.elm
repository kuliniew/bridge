module Solver.IntervalTests exposing (all)

import Solver.Interval
import TestUtils

import Check
import Check.Producer
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Interval"
    [ emptySuite
    , unboundedSuite
    ]


emptySuite : ElmTest.Test
emptySuite =
  ElmTest.suite "empty"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains no values"
        `Check.false`
          flip Solver.Interval.member Solver.Interval.empty
        `Check.for`
          Check.Producer.int
    ]


unboundedSuite : ElmTest.Test
unboundedSuite =
  ElmTest.suite "unbounded"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains all values"
        `Check.true`
          flip Solver.Interval.member Solver.Interval.unbounded
        `Check.for`
          Check.Producer.int
    ]
