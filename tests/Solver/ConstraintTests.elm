module Solver.ConstraintTests exposing (all)

import Solver.Constraint
import Solver.Range
import Solver.Term

import ElmTest
import EveryDict exposing (EveryDict)
import List.Extra


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Constraint"
    [ evaluateSuite
    , boundVariablesSuite
    ]


evaluateSuite : ElmTest.Test
evaluateSuite =
  ElmTest.suite "evaluate"
    [ ElmTest.test "1 = 1" <|
        let
          variables =
            EveryDict.empty
          constraint =
            Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1
        in
          ElmTest.assertEqual (Just variables) (Solver.Constraint.evaluate variables constraint)

    , ElmTest.test "1 = 2" <|
        let
          variables =
            EveryDict.empty
          constraint =
            Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2
        in
          ElmTest.assertEqual Nothing (Solver.Constraint.evaluate variables constraint)

    , ElmTest.test "x = 1" <|
        let
          variables =
            EveryDict.empty
          constraint =
            Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1
          expected =
            EveryDict.singleton "x" (Solver.Range.singleton 1)
        in
          ElmTest.assertEqual (Just expected) (Solver.Constraint.evaluate variables constraint)
    ]


boundVariablesSuite : ElmTest.Test
boundVariablesSuite =
  ElmTest.suite "boundVariables"
    [ ElmTest.test "1 = 1" <|
        let
          constraint =
            Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1
        in
          ElmTest.assertEqual [] (EveryDict.keys <| Solver.Constraint.boundVariables constraint)

    , ElmTest.test "x = 1" <|
        let
          constraint =
            Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1
        in
          ElmTest.assertEqual ["x"] (EveryDict.keys <| Solver.Constraint.boundVariables constraint)

    , ElmTest.test "x = x" <|
        let
          constraint =
            Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "x"
        in
          ElmTest.assertEqual ["x"] (EveryDict.keys <| Solver.Constraint.boundVariables constraint)

    , ElmTest.test "x = y" <|
        let
          constraint =
            Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "y"
        in
          ElmTest.assert <| List.Extra.isPermutationOf ["x", "y"] (EveryDict.keys <| Solver.Constraint.boundVariables constraint)
    ]
