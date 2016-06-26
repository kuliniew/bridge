module Solver.ConstraintTests exposing (all)

import Solver.Constraint
import Solver.Term

import ElmTest
import EveryDict exposing (EveryDict)


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Constraint"
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
    ]
