module Solver.ProblemTests exposing (all)

import Solver.Problem
import Solver.Range
import TestUtils

import Check
import Check.Producer
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Problem"
    [ emptySuite
    ]


emptySuite : ElmTest.Test
emptySuite =
  ElmTest.suite "empty"
    [ ElmTest.test "is solvable" <|
        ElmTest.assert <| Solver.Problem.isSolvable Solver.Problem.empty

    , TestUtils.generativeTest <|
        Check.claim
          "allows all possible values for any variable"
        `Check.that`
          flip Solver.Problem.possibleValues Solver.Problem.empty
        `Check.is`
          always Solver.Range.full
        `Check.for`
          Check.Producer.string
    ]
