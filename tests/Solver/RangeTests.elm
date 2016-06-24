module Solver.RangeTests exposing (all)

import Solver.Range
import TestUtils

import Check
import Check.Producer
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Range"
    [ emptySuite
    , fullSuite
    ]


emptySuite : ElmTest.Test
emptySuite =
  ElmTest.suite "empty"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains no values"
        `Check.false`
          flip Solver.Range.member Solver.Range.empty
        `Check.for`
          Check.Producer.int

    , ElmTest.test "is empty" <|
        ElmTest.assert <| Solver.Range.isEmpty Solver.Range.empty
    ]


fullSuite : ElmTest.Test
fullSuite =
  ElmTest.suite "full"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains all values"
        `Check.true`
          flip Solver.Range.member Solver.Range.full
        `Check.for`
          Check.Producer.int

    , ElmTest.test "is not empty" <|
        ElmTest.assert <| not <| Solver.Range.isEmpty Solver.Range.full
    ]
