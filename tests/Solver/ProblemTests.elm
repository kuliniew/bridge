module Solver.ProblemTests exposing (all)

import Solver.Constraint
import Solver.Problem
import Solver.Range
import Solver.Term
import TestUtils

import Check
import Check.Producer
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Problem"
    [ emptySuite
    , constantTautologySuite
    , constantFallacySuite
    , singleVariableEqualitySuite
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


constantTautologySuite : ElmTest.Test
constantTautologySuite =
  let
    problem =
      Solver.Problem.empty
        |> Solver.Problem.addConstraint (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1)
  in
    ElmTest.suite "constant tautology"
      [ ElmTest.test "is solvable" <|
          ElmTest.assert <| Solver.Problem.isSolvable problem

      , TestUtils.generativeTest <|
          Check.claim
            "allows all possible values for any variable"
          `Check.that`
          flip Solver.Problem.possibleValues problem
        `Check.is`
          always Solver.Range.full
        `Check.for`
          Check.Producer.string
      ]


constantFallacySuite : ElmTest.Test
constantFallacySuite =
  let
    problem =
      Solver.Problem.empty
        |> Solver.Problem.addConstraint (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2)
  in
    ElmTest.suite "constant fallacy"
      [ ElmTest.test "is not solvable" <|
          ElmTest.assert <| not <| Solver.Problem.isSolvable problem

      , TestUtils.generativeTest <|
          Check.claim
            "allows no possible values for any variable"
          `Check.that`
          flip Solver.Problem.possibleValues problem
        `Check.is`
          always Solver.Range.empty
        `Check.for`
          Check.Producer.string
      ]


singleVariableEqualitySuite : ElmTest.Test
singleVariableEqualitySuite =
  let
    problem =
      Solver.Problem.empty
        |> Solver.Problem.addConstraint (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
  in
    ElmTest.suite "single variable equality"
      [ ElmTest.test "is solvable" <|
          ElmTest.assert <| Solver.Problem.isSolvable problem

      , ElmTest.test "constrained variable's value is known" <|
          ElmTest.assertEqual (Solver.Range.singleton 1) (Solver.Problem.possibleValues "x" problem)

      , TestUtils.generativeTest <|
          Check.claim
            "other variables are unconstrained"
          `Check.that`
            flip Solver.Problem.possibleValues problem
          `Check.is`
            always Solver.Range.full
          `Check.for`
            Check.Producer.filter
              ((/=) "x")
              Check.Producer.string
      ]
