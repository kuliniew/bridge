module Solver.ConstraintTests exposing (all)

import Solver.Constraint exposing (Constraint)
import Solver.Endpoint
import Solver.Interval exposing (Interval)
import Solver.Range exposing (Range)
import Solver.Term
import Solver.TermTests exposing (termProducer, variableProducer, variablesProducer)
import TestUtils

import Check
import Check.Producer
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
    , relationsSuite
    , andSuite
    , allSuite
    , orSuite
    , anySuite
    , notSuite
    , ifThenSuite
    , ifThenElseSuite
    , logicSuite
    , decomposeSuite
    ]


equalSuite : ElmTest.Test
equalSuite =
  ElmTest.suite "equal"
    [ commutativeTests Solver.Constraint.equal termProducer

    , evaluateTestCase
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

    , evaluateTestCase
        "x = x"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "x")
        (Just EveryDict.empty)

    , evaluateTestCase
        "x = x + 1"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.equal` (Solver.Term.variable "x" `Solver.Term.add` Solver.Term.constant 1))
        Nothing

    , evaluateTestCase
        "x = x + x"
        EveryDict.empty
        (Solver.Term.variable "x" `Solver.Constraint.equal` (Solver.Term.variable "x" `Solver.Term.add` Solver.Term.variable "x"))
        (Just <| EveryDict.singleton "x" (Solver.Range.singleton 0))

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
        []   -- because x cancels out

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


relationsSuite : ElmTest.Test
relationsSuite =
  let
    flippedTests name1 operation1 name2 operation2 =
      equivalentTests
        ("x " ++ name1 ++ " y  <-->  y " ++ name2 ++ " x")
        (uncurry operation1)
        (uncurry <| flip operation2)
        (Check.Producer.tuple (termProducer, termProducer))
    negatedTests name1 operation1 name2 operation2 =
      equivalentTests
        ("x " ++ name1 ++ " y  <-->  !(x " ++ name2 ++ " y)")
        (uncurry operation1)
        (\(x, y) -> Solver.Constraint.not (x `operation2` y))
        (Check.Producer.tuple (termProducer, termProducer))
  in
    ElmTest.suite "relations"
      [ flippedTests "<=" Solver.Constraint.lessThanOrEqual ">=" Solver.Constraint.greaterThanOrEqual
      , flippedTests "<" Solver.Constraint.lessThan ">" Solver.Constraint.greaterThan

      , negatedTests "<=" Solver.Constraint.lessThanOrEqual ">" Solver.Constraint.greaterThan
      , negatedTests "<" Solver.Constraint.lessThan ">" Solver.Constraint.greaterThanOrEqual
      , negatedTests ">=" Solver.Constraint.greaterThanOrEqual "<" Solver.Constraint.lessThan
      , negatedTests ">" Solver.Constraint.greaterThan "<=" Solver.Constraint.lessThanOrEqual
      ]


andSuite : ElmTest.Test
andSuite =
  ElmTest.suite "and"
    [ commutativeTests Solver.Constraint.and constraintProducer

    , associativeTests Solver.Constraint.and

    , idempotentTests Solver.Constraint.and

    , evaluateTestCase
        "1 < 2 && 2 < 3"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3))
        (Just EveryDict.empty)

    , evaluateTestCase
        "1 > 2 && 2 < 3"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3))
        Nothing

    , evaluateTestCase
        "1 < 2 && 2 > 3"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.greaterThan` Solver.Term.constant 3))
        Nothing

    , evaluateTestCase
        "1 > 2 && 2 > 3"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.greaterThan` Solver.Term.constant 3))
        Nothing

    , evaluateTestCase
        "1 <= x && x <= 10"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x")
          (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 10))
        (Just <| EveryDict.singleton "x" (Solver.Range.fromIntervals [boundedInterval 1 10]))

    , evaluateTestCase
        "x = y && x = 1"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.variable "y")
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1))
        (Just <| EveryDict.fromList
          [ ("x", Solver.Range.singleton 1)
          , ("y", Solver.Range.singleton 1)
          ])

    , boundVariablesTestCase
        "w < x && y < z"
        (Solver.Constraint.and
          (Solver.Term.variable "w" `Solver.Constraint.lessThan` Solver.Term.variable "x")
          (Solver.Term.variable "y" `Solver.Constraint.lessThan` Solver.Term.variable "z"))
        ["w", "x", "y", "z"]
    ]


allSuite : ElmTest.Test
allSuite =
  ElmTest.suite "all"
    [ evaluateTestCase
        "[]"
        EveryDict.empty
        (Solver.Constraint.all [])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 < 2]"
        EveryDict.empty
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 < 2, 2 < 3]"
        EveryDict.empty
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 < 2, 2 < 3, 3 < 4]"
        EveryDict.empty
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3
          , Solver.Term.constant 3 `Solver.Constraint.lessThan` Solver.Term.constant 4
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 < 2, 2 < 3, 3 < 4, 4 < 5]"
        EveryDict.empty
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3
          , Solver.Term.constant 3 `Solver.Constraint.lessThan` Solver.Term.constant 4
          , Solver.Term.constant 4 `Solver.Constraint.lessThan` Solver.Term.constant 5
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 < 2, 2 < 3, 3 > 4, 4 < 5]"
        EveryDict.empty
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3
          , Solver.Term.constant 3 `Solver.Constraint.greaterThan` Solver.Term.constant 4
          , Solver.Term.constant 4 `Solver.Constraint.lessThan` Solver.Term.constant 5
          ])
        Nothing

    , evaluateTestCase
        "[1 <= x, x <= 10, 1 <= y, y <= 5]"
        EveryDict.empty
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x"
          , Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 10
          , Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "y"
          , Solver.Term.variable "y" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 5
          ])
        (Just <| EveryDict.fromList
          [ ("x", Solver.Range.fromIntervals [boundedInterval 1 10])
          , ("y", Solver.Range.fromIntervals [boundedInterval 1 5])
          ])

    , boundVariablesTestCase
        "[]"
        (Solver.Constraint.all [])
        []

    , boundVariablesTestCase
        "[1 <= x, x <= 10, 1 <= y, y <= 5]"
        (Solver.Constraint.all
          [ Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x"
          , Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 10
          , Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "y"
          , Solver.Term.variable "y" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 5
          ])
        ["x", "y"]
    ]


orSuite : ElmTest.Test
orSuite =
  ElmTest.suite "or"
    [ commutativeTests Solver.Constraint.or constraintProducer

    , associativeTests Solver.Constraint.or

    , idempotentTests Solver.Constraint.and

    , evaluateTestCase
        "1 < 2 || 2 < 3"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3))
        (Just EveryDict.empty)

    , evaluateTestCase
        "1 > 2 || 2 < 3"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.lessThan` Solver.Term.constant 3))
        (Just EveryDict.empty)

    , evaluateTestCase
        "1 < 2 || 2 > 3"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.constant 1 `Solver.Constraint.lessThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.greaterThan` Solver.Term.constant 3))
        (Just EveryDict.empty)

    , evaluateTestCase
        "1 > 2 || 2 > 3"
        EveryDict.empty
        (Solver.Constraint.and
          (Solver.Term.constant 1 `Solver.Constraint.greaterThan` Solver.Term.constant 2)
          (Solver.Term.constant 2 `Solver.Constraint.greaterThan` Solver.Term.constant 3))
        Nothing

    , evaluateTestCase
        "x <= 1 || x >= 10"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 1)
          (Solver.Term.variable "x" `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 10))
        (Just <| EveryDict.singleton "x" (Solver.Range.union (Solver.Range.fromUpperBound 1) (Solver.Range.fromLowerBound 10)))

    , evaluateTestCase
        "x >= 1 || y >= 1"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.variable "x" `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 1)
          (Solver.Term.variable "y" `Solver.Constraint.greaterThanOrEqual` Solver.Term.constant 1))
        (Just <| EveryDict.fromList
          [ ("x", Solver.Range.full)
          , ("y", Solver.Range.full)
          ])

    , evaluateTestCase
        "1 = 2 || x = 1"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2)
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1))
        (Just <| EveryDict.singleton "x" (Solver.Range.singleton 1))

    , evaluateTestCase
        "x = 1 || 1 = 2"
        EveryDict.empty
        (Solver.Constraint.or
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
          (Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2))
        (Just <| EveryDict.singleton "x" (Solver.Range.singleton 1))

    , boundVariablesTestCase
        "w < x || y < z"
        (Solver.Constraint.or
          (Solver.Term.variable "w" `Solver.Constraint.lessThan` Solver.Term.variable "x")
          (Solver.Term.variable "y" `Solver.Constraint.lessThan` Solver.Term.variable "z"))
        ["w", "x", "y", "z"]
    ]


anySuite : ElmTest.Test
anySuite =
  ElmTest.suite "any"
    [ evaluateTestCase
        "[]"
        EveryDict.empty
        (Solver.Constraint.any [])
        Nothing

    , evaluateTestCase
        "[1 = 1]"
        EveryDict.empty
        (Solver.Constraint.any
          [ Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 = 2, 2 = 2]"
        EveryDict.empty
        (Solver.Constraint.any
          [ Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.equal` Solver.Term.constant 2
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 = 2, 2 = 3, 3 = 3]"
        EveryDict.empty
        (Solver.Constraint.any
          [ Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.equal` Solver.Term.constant 3
          , Solver.Term.constant 3 `Solver.Constraint.equal` Solver.Term.constant 3
          ])
        (Just EveryDict.empty)

    , evaluateTestCase
        "[1 = 2, 2 = 3, 3 = 4]"
        EveryDict.empty
        (Solver.Constraint.any
          [ Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2
          , Solver.Term.constant 2 `Solver.Constraint.equal` Solver.Term.constant 3
          , Solver.Term.constant 3 `Solver.Constraint.equal` Solver.Term.constant 4
          ])
        Nothing

    , evaluateTestCase
        "[1 = 2, x = 3, x = 4]"
        EveryDict.empty
        (Solver.Constraint.any
          [ Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2
          , Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 3
          , Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 4
          ])
        (Just <| EveryDict.singleton "x" (Solver.Range.fromIntervals [boundedInterval 3 4]))

    , boundVariablesTestCase
        "[]"
        (Solver.Constraint.any [])
        []

    , boundVariablesTestCase
        "[1 <= x, x <= 10, 1 <= y, y <= 5]"
        (Solver.Constraint.any
          [ Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "x"
          , Solver.Term.variable "x" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 10
          , Solver.Term.constant 1 `Solver.Constraint.lessThanOrEqual` Solver.Term.variable "y"
          , Solver.Term.variable "y" `Solver.Constraint.lessThanOrEqual` Solver.Term.constant 5
          ])
        ["x", "y"]
    ]


notSuite : ElmTest.Test
notSuite =
  ElmTest.suite "not"
    [ evaluateTestCase
        "!(x == 5)"
        EveryDict.empty
        (Solver.Constraint.not
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 5))
        (Just <| EveryDict.singleton "x" (Solver.Range.union (Solver.Range.fromUpperBound 4) (Solver.Range.fromLowerBound 6)))

    , boundVariablesTestCase
        "!(x == 5)"
        (Solver.Constraint.not
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 5))
        ["x"]

    , equivalentTests
        "!!x  <-->  x"
        (Solver.Constraint.not << Solver.Constraint.not)
        identity
        constraintProducer

    , TestUtils.generativeTest <|
        Check.claim
          "binds the same variables as its subconstraint"
        `Check.that`
          (boundVariablesAsList << Solver.Constraint.not)
        `Check.is`
          boundVariablesAsList
        `Check.for`
          constraintProducer
    ]


ifThenSuite : ElmTest.Test
ifThenSuite =
  ElmTest.suite "ifThen"
    [ equivalentTests
        "true -> x  <-->  x"
        (Solver.Constraint.ifThen alwaysTrue)
        identity
        constraintProducer

    , equivalentTests
        "x -> false  <-->  !x"
        (flip Solver.Constraint.ifThen alwaysFalse)
        Solver.Constraint.not
        constraintProducer

    , equivalentTests
        "x -> y  <-->  !y -> !x"
        (uncurry Solver.Constraint.ifThen)
        (\(x, y) -> Solver.Constraint.ifThen (Solver.Constraint.not y) (Solver.Constraint.not x))
        (Check.Producer.tuple (constraintProducer, constraintProducer))
    ]


ifThenElseSuite : ElmTest.Test
ifThenElseSuite =
  ElmTest.suite "ifThenElse"
    [ evaluateTestCase
        "only then clause"
        EveryDict.empty
        (Solver.Constraint.ifThenElse
          alwaysTrue
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 2))
        (Just <| EveryDict.singleton "x" (Solver.Range.singleton 1))

    , evaluateTestCase
        "only else clause"
        EveryDict.empty
        (Solver.Constraint.ifThenElse
          alwaysFalse
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 2))
        (Just <| EveryDict.singleton "x" (Solver.Range.singleton 2))

    , evaluateTestCase
        "indefinite"
        EveryDict.empty
        (Solver.Constraint.ifThenElse
          (Solver.Term.variable "y" `Solver.Constraint.equal` Solver.Term.constant 1)
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 1)
          (Solver.Term.variable "x" `Solver.Constraint.equal` Solver.Term.constant 2))
        (Just <| EveryDict.fromList
          [ ("x", Solver.Range.fromIntervals [boundedInterval 1 2])
          , ("y", Solver.Range.full)
          ])
    ]


logicSuite : ElmTest.Test
logicSuite =
  let
    distributiveTests name1 operation1 name2 operation2 =
      equivalentTests
        ("x " ++ name1 ++ " (y " ++ name2 ++ " z)  <-->  (x " ++ name1 ++ " y) " ++ name2 ++ " (x " ++ name1 ++ " z)")
        (\(x, y, z) -> x `operation1` (y `operation2` z))
        (\(x, y, z) -> (x `operation1` y) `operation2` (x `operation1` z))
        (Check.Producer.tuple3 (constraintProducer, constraintProducer, constraintProducer))
    deMorganTests name1 operation1 name2 operation2 =
      equivalentTests
        ("!(x " ++ name1 ++ " y)  <-->  (!x) " ++ name2 ++ " (!y)")
        (\(x, y) -> Solver.Constraint.not (x `operation1` y))
        (\(x, y) -> (Solver.Constraint.not x) `operation2` (Solver.Constraint.not y))
        (Check.Producer.tuple (constraintProducer, constraintProducer))
  in
    ElmTest.suite "logic"
      [ distributiveTests "&&" Solver.Constraint.and "||" Solver.Constraint.or
      , distributiveTests "||" Solver.Constraint.or "&&" Solver.Constraint.and

      , deMorganTests "&&" Solver.Constraint.and "||" Solver.Constraint.or
      , deMorganTests "||" Solver.Constraint.or "&&" Solver.Constraint.and

      , TestUtils.generativeTest <|
          Check.claim
            "tautology"
          `Check.that`
            (\(constraint, variables) -> evaluateAsList variables <| constraint `Solver.Constraint.or` (Solver.Constraint.not constraint))
          `Check.is`
            (\(_, variables) -> Just <| EveryDict.toList variables)
          `Check.for`
            (Check.Producer.tuple (constraintProducer, variablesProducer))

      {-
        This test doesn't really work unless a conjunction were to evaluate each subconstraint simultaneously.
        For example, in "x + y > 0 && x + y < 0", where x and y are initially unbounded, neither subconstraint
        is able by itself to tighten the bounds of x or y, even though there is no individual choice of x or y
        that can satisfy both subconstraints simultaneously.

        There are hacks possible to handle this particular case, but in general the way the solver works can't
        really handle situations like this well.
      -}
      {-
      , TestUtils.generativeTest <|
          Check.claim
            "fallacy"
          `Check.that`
            (\(constraint, variables) -> evaluateAsList variables <| constraint `Solver.Constraint.and` (Solver.Constraint.not constraint))
          `Check.is`
            always Nothing
          `Check.for`
            (Check.Producer.tuple (constraintProducer, variablesProducer))
      -}
      ]


decomposeSuite : ElmTest.Test
decomposeSuite =
  ElmTest.suite "decompose"
    [ TestUtils.generativeTest <|
        Check.claim
          "equivalent to original constraint"
        `Check.that`
          (\(variables, constraint) ->
            Solver.Constraint.decompose constraint
              |> List.foldl (\con mvar -> mvar `Maybe.andThen` flip Solver.Constraint.evaluate con) (Just variables)
              |> Maybe.map EveryDict.toList)
        `Check.is`
          uncurry evaluateAsList
        `Check.for`
          Check.Producer.tuple (variablesProducer, constraintProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "decomposes conjunctions"
        `Check.true`
          (\(x, y) -> List.length (Solver.Constraint.decompose (x `Solver.Constraint.and` y)) >= 2)
        `Check.for`
          Check.Producer.tuple (constraintProducer, constraintProducer)
    ]


commutativeTests : (a -> a -> Constraint String) -> Check.Producer.Producer a -> ElmTest.Test
commutativeTests operation argProducer =
  equivalentTests
    "commutative"
    (uncurry operation)
    (uncurry <| flip operation)
    (Check.Producer.tuple (argProducer, argProducer))


associativeTests : (Constraint String -> Constraint String -> Constraint String) -> ElmTest.Test
associativeTests operation =
  equivalentTests
    "associative"
    (\(x, y, z) -> x `operation` (y `operation` z))
    (\(x, y, z) -> (x `operation` y) `operation` z)
    (Check.Producer.tuple3 (constraintProducer, constraintProducer, constraintProducer))


idempotentTests : (Constraint String -> Constraint String -> Constraint String) -> ElmTest.Test
idempotentTests operation =
  equivalentTests
    "idempotent"
    (\x -> x `operation` x)
    identity
    constraintProducer


equivalentTests : String -> (a -> Constraint String) -> (a -> Constraint String) -> Check.Producer.Producer a -> ElmTest.Test
equivalentTests name builder1 builder2 argsProducer =
  ElmTest.suite name
    [ TestUtils.generativeTest <|
        Check.claim
          "evaluate"
        `Check.that`
          (\(args, variables) -> evaluateAsList variables <| builder1 args)
        `Check.is`
          (\(args, variables) -> evaluateAsList variables <| builder2 args)
        `Check.for`
          Check.Producer.tuple (argsProducer, variablesProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "boundVariables"
        `Check.that`
          (boundVariablesAsList << builder1)
        `Check.is`
          (boundVariablesAsList << builder2)
        `Check.for`
          argsProducer
    ]


evaluateTestCase : String -> EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range) -> ElmTest.Test
evaluateTestCase name initial constraint expected =
  ElmTest.test ("evaluate: " ++ name) <|
    ElmTest.assertEqual
      (Maybe.map EveryDict.toList expected)
      (evaluateAsList initial constraint)


evaluateAsList : EveryDict var Range -> Constraint var -> Maybe (List (var, Range))
evaluateAsList variables constraint =
  Maybe.map EveryDict.toList <| Solver.Constraint.evaluate variables constraint


boundVariablesTestCase : String -> Constraint var -> List var -> ElmTest.Test
boundVariablesTestCase name constraint expected =
  ElmTest.test ("boundVariables: " ++ name) <|
    ElmTest.assertEqual expected (boundVariablesAsList constraint)


boundVariablesAsList : Constraint var -> List var
boundVariablesAsList =
  EveryDict.keys << Solver.Constraint.boundVariables


boundedInterval : Int -> Int -> Interval
boundedInterval lo hi =
  case Solver.Interval.fromEndpoints (Solver.Endpoint.Point lo) (Solver.Endpoint.Point hi) of
    Just interval ->
      interval
    Nothing ->
      Debug.crash <| "boundedInterval failed for: " ++ toString lo ++ " " ++ toString hi


constraintProducer : Check.Producer.Producer (Constraint String)
constraintProducer =
  Solver.Constraint.producer variableProducer


alwaysTrue : Constraint var
alwaysTrue =
  Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 1


alwaysFalse : Constraint var
alwaysFalse =
  Solver.Term.constant 1 `Solver.Constraint.equal` Solver.Term.constant 2
