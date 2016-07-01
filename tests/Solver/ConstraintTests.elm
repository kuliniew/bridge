module Solver.ConstraintTests exposing (all)

import Solver.Constraint
import Solver.Range
import Solver.Term

import ElmTest
import EveryDict exposing (EveryDict)


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Constraint"
    [ evaluateSuite
    , boundVariablesSuite
    ]


evaluateSuite : ElmTest.Test
evaluateSuite =
  ElmTest.suite "evaluate" <|
    let
      testCase name initial constraint expected =
        ElmTest.test name <|
          ElmTest.assertEqual expected (Solver.Constraint.evaluate initial constraint)
    in
      [ testCase
          "1 = 1"
          EveryDict.empty
          (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1)
          (Just EveryDict.empty)

      , testCase
          "1 = 2"
          EveryDict.empty
          (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2)
          Nothing

      , testCase
          "x = 1"
          EveryDict.empty
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
          (Just <| EveryDict.singleton "x" (Solver.Range.singleton 1))

      , testCase
          "1 <= 2"
          EveryDict.empty
          (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 2)
          (Just EveryDict.empty)

      , testCase
          "2 <= 1"
          EveryDict.empty
          (Solver.Term.constant 2 `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
          Nothing

      , testCase
          "x <= 1"
          EveryDict.empty
          (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
          (Just <| EveryDict.singleton "x" (Solver.Range.fromUpperBound 1))

      , testCase
          "1 <= x"
          EveryDict.empty
          (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x")
          (Just <| EveryDict.singleton "x" (Solver.Range.fromLowerBound 1))
      ]


boundVariablesSuite : ElmTest.Test
boundVariablesSuite =
  ElmTest.suite "boundVariables" <|
    let
      testCase name constraint expected =
        ElmTest.test name <|
          ElmTest.assertEqual expected (EveryDict.keys <| Solver.Constraint.boundVariables constraint)
    in
      [ testCase
          "1 = 1"
          (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1)
          []

      , testCase
          "x = 1"
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
          ["x"]

      , testCase
          "x = x"
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "x")
          ["x"]

      , testCase
          "x = y"
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "y")
          ["x", "y"]

      , testCase
          "1 <= 2"
          (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 2)
          []

      , testCase
          "x <= 1"
          (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
          ["x"]

      , testCase
          "1 <= x"
          (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x")
          ["x"]

      , testCase
          "x <= y"
          (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "y")
          ["x", "y"]
      ]
