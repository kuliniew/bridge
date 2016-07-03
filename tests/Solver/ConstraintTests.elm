module Solver.ConstraintTests exposing (all)

import Solver.Constraint exposing (Constraint)
import Solver.Range exposing (Range)
import Solver.Term

import ElmTest
import EveryDict exposing (EveryDict)


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Constraint"
    [ equalSuite
    , lessThanOrEqualSuite
    , lessThanSuite
    , greaterThanOrEqualSuite
    , greaterThanSuite
    ]


equalSuite : ElmTest.Test
equalSuite =
  ElmTest.suite "equal"
    [ evaluateTestCase
        "1 = 1"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1)
        (Just EveryDict.empty)

    , evaluateTestCase
        "1 = 2"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2)
        Nothing

    , evaluateTestCase
        "x = 1"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
        (Just <| EveryDict.singleton "x" (Solver.Range.singleton 1))

    , boundVariablesTestCase
        "1 = 1"
        (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1)
        []

    , boundVariablesTestCase
        "x = 1"
        (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
        ["x"]

    , boundVariablesTestCase
        "x = x"
        (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "x")
        ["x"]

    , boundVariablesTestCase
        "x = y"
        (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "y")
        ["x", "y"]
    ]


lessThanOrEqualSuite : ElmTest.Test
lessThanOrEqualSuite =
  ElmTest.suite "lessThanOrEqual"
    [ evaluateTestCase
        "1 <= 2"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 2)
        (Just EveryDict.empty)

    , evaluateTestCase
        "2 <= 1"
        EveryDict.empty
        (Solver.Term.constant 2 `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
        Nothing

    , evaluateTestCase
        "x <= 1"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
        (Just <| EveryDict.singleton "x" (Solver.Range.fromUpperBound 1))

    , evaluateTestCase
        "1 <= x"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x")
        (Just <| EveryDict.singleton "x" (Solver.Range.fromLowerBound 1))

    , boundVariablesTestCase
        "1 <= 2"
        (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 2)
        []

    , boundVariablesTestCase
        "x <= 1"
        (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
        ["x"]

    , boundVariablesTestCase
        "1 <= x"
        (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x")
        ["x"]

    , boundVariablesTestCase
        "x <= y"
        (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "y")
        ["x", "y"]
    ]


lessThanSuite : ElmTest.Test
lessThanSuite =
  ElmTest.suite "lessThan"
    [ evaluateTestCase
        "1 < 2"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2)
        (Just EveryDict.empty)

    , evaluateTestCase
        "1 < 1"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 1)
        Nothing

    , evaluateTestCase
        "x < 1"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.lessThan` Solver.Term.constant 1)
        (Just <| EveryDict.singleton "x" (Solver.Range.fromUpperBound 0))

    , evaluateTestCase
        "1 < x"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.variable "x")
        (Just <| EveryDict.singleton "x" (Solver.Range.fromLowerBound 2))

    , boundVariablesTestCase
        "1 < 2"
        (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2)
        []

    , boundVariablesTestCase
        "x < 1"
        (Solver.Term.variable "x" `Solver.Constraint.lessThan` Solver.Term.constant 1)
        ["x"]

    , boundVariablesTestCase
        "1 < x"
        (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.variable "x")
        ["x"]

    , boundVariablesTestCase
        "x < y"
        (Solver.Term.variable "x" `Solver.Constraint.lessThan` Solver.Term.variable "y")
        ["x", "y"]
    ]


greaterThanOrEqualSuite : ElmTest.Test
greaterThanOrEqualSuite =
  ElmTest.suite "greaterThanOrEqual"
    [ evaluateTestCase
        "1 >= 2"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 2)
        Nothing

    , evaluateTestCase
        "2 >= 1"
        EveryDict.empty
        (Solver.Term.constant 2 `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 1)
        (Just EveryDict.empty)

    , evaluateTestCase
        "x >= 1"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 1)
        (Just <| EveryDict.singleton "x" (Solver.Range.fromLowerBound 1))

    , evaluateTestCase
        "1 >= x"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.greaterThanOrEqual` Solver.Term.variable "x")
        (Just <| EveryDict.singleton "x" (Solver.Range.fromUpperBound 1))

    , boundVariablesTestCase
        "1 >= 2"
        (Solver.Term.constant 1 `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 2)
        []

    , boundVariablesTestCase
        "x >= 1"
        (Solver.Term.variable "x" `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 1)
        ["x"]

    , boundVariablesTestCase
        "1 >= x"
        (Solver.Term.constant 1 `Solver.Constraint.greaterThanOrEqual` Solver.Term.variable "x")
        ["x"]

    , boundVariablesTestCase
        "x >= y"
        (Solver.Term.variable "x" `Solver.Constraint.greaterThanOrEqual` Solver.Term.variable "y")
        ["x", "y"]
    ]


greaterThanSuite : ElmTest.Test
greaterThanSuite =
  ElmTest.suite "greaterThan" <|
    [ evaluateTestCase
        "1 > 2"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 2)
        Nothing

    , evaluateTestCase
        "1 > 1"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 1)
        Nothing

    , evaluateTestCase
        "x > 1"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.greaterThan` Solver.Term.constant 1)
        (Just <| EveryDict.singleton "x" (Solver.Range.fromLowerBound 2))

    , evaluateTestCase
        "1 > x"
        EveryDict.empty
        (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.variable "x")
        (Just <| EveryDict.singleton "x" (Solver.Range.fromUpperBound 0))

    , boundVariablesTestCase
        "1 > 2"
        (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 2)
        []

    , boundVariablesTestCase
        "x > 1"
        (Solver.Term.variable "x" `Solver.Constraint.greaterThan` Solver.Term.constant 1)
        ["x"]

    , boundVariablesTestCase
        "1 > x"
        (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.variable "x")
        ["x"]

    , boundVariablesTestCase
        "x > y"
        (Solver.Term.variable "x" `Solver.Constraint.greaterThan` Solver.Term.variable "y")
        ["x", "y"]
    ]


evaluateTestCase : String -> EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range) -> ElmTest.Test
evaluateTestCase name initial constraint expected =
  ElmTest.test name <|
    ElmTest.assertEqual expected (Solver.Constraint.evaluate initial constraint)


boundVariablesTestCase : String -> Constraint var -> List var -> ElmTest.Test
boundVariablesTestCase name constraint expected =
  ElmTest.test name <|
    ElmTest.assertEqual expected (EveryDict.keys <| Solver.Constraint.boundVariables constraint)
