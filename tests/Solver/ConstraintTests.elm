module Solver.ConstraintTests exposing (all)

import Solver.Constraint exposing (Constraint)
import Solver.Endpoint
import Solver.Interval exposing (Interval)
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
    , andSuite
    , allSuite
    , orSuite
    , anySuite
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


andSuite : ElmTest.Test
andSuite =
  ElmTest.suite "and"
    [ evaluateTestCase
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
    [ evaluateTestCase
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


evaluateTestCase : String -> EveryDict var Range -> Constraint var -> Maybe (EveryDict var Range) -> ElmTest.Test
evaluateTestCase name initial constraint expected =
  ElmTest.test name <|
    ElmTest.assertEqual
      (Maybe.map EveryDict.toList expected)
      (Maybe.map EveryDict.toList <| Solver.Constraint.evaluate initial constraint)


boundVariablesTestCase : String -> Constraint var -> List var -> ElmTest.Test
boundVariablesTestCase name constraint expected =
  ElmTest.test name <|
    ElmTest.assertEqual expected (EveryDict.keys <| Solver.Constraint.boundVariables constraint)


boundedInterval : Int -> Int -> Interval
boundedInterval lo hi =
  case Solver.Interval.fromEndpoints (Solver.Endpoint.Point lo) (Solver.Endpoint.Point hi) of
    Just interval ->
      interval
    Nothing ->
      Debug.crash <| "boundedInterval failed for: " ++ toString lo ++ " " ++ toString hi
