module ConstraintTests exposing (all)

import Constraint

import ElmTest
import Set exposing (Set)


all : ElmTest.Test
all =
  ElmTest.suite "Constraint"
    [ possibleValuesSuite
    , rangeSuite
    , constrainSuite
    ]


possibleValuesSuite : ElmTest.Test
possibleValuesSuite =
  let
    state =
      Constraint.initialize
        [ ("foo", Set.fromList [1, 2, 3])
        , ("bar", Set.fromList [2, 3, 4])
        ]
  in
    ElmTest.suite "possibleValues"
      [ ElmTest.test "of initialized variable" <|
          ElmTest.assertEqual (Set.fromList [1, 2, 3]) (Constraint.possibleValues "foo" state)

      , ElmTest.test "of uninitialized variable" <|
          ElmTest.assertEqual Set.empty (Constraint.possibleValues "missing" state)
      ]


rangeSuite : ElmTest.Test
rangeSuite =
  let
    range = Constraint.range 1 10
  in
    ElmTest.suite "range"
      [ ElmTest.test "contains lower bound" <|
          ElmTest.assert (Set.member 1 range)

      , ElmTest.test "contains upper bound" <|
          ElmTest.assert (Set.member 10 range)

      , ElmTest.test "contains inner values" <|
          ElmTest.assert (Set.member 5 range)

      , ElmTest.test "excludes values less than lower bound" <|
          ElmTest.assert (not <| Set.member 0 range)

      , ElmTest.test "excludes values greater than upper bound" <|
          ElmTest.assert (not <| Set.member 11 range)
      ]


constrainSuite : ElmTest.Test
constrainSuite =
  ElmTest.suite "constrain"
    [ constrainTest
        "x < 5"
        [ Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 5) ]
        [  ("x", Constraint.range 1 4) ]

    , constrainTest
        "5 < x"
        [ Constraint.LessThan (Constraint.Constant 5) (Constraint.Variable "x") ]
        [ ("x", Constraint.range 6 10) ]

    , constrainTest
        "x + y + z < 6"
        [ Constraint.LessThan
            (Constraint.Add <| List.map Constraint.Variable ["x", "y", "z"])
            (Constraint.Constant 6)
        ]
        ( List.map (\var -> (var, Constraint.range 1 3)) ["x", "y", "z"] )

    , constrainTest
        "(x < 3) || (7 < x)"
        [ Constraint.Or
            [ Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 3)
            , Constraint.LessThan (Constraint.Constant 7) (Constraint.Variable "x")
            ]
        ]
        [ ("x", Set.fromList [1, 2, 8, 9, 10]) ]

    , constrainTest
        "missing"
        [ Constraint.LessThan (Constraint.Variable "missing") (Constraint.Constant 5) ]
        [ ("missing", Set.empty) ]

    , constrainTest
        "(3 < x) ; (x < 8)"
        [ Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x")
        , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8)
        ]
        [ ("x", Constraint.range 4 7) ]

    , constrainTest
        "(x < 8) ; (y < x)"
        [ Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8)
        , Constraint.LessThan (Constraint.Variable "y") (Constraint.Variable "x")
        ]
        [ ("x", Constraint.range 2 7)
        , ("y", Constraint.range 1 6)
        ]

    , constrainTest
        "(y < x) ; (x < 8)"
        [ Constraint.LessThan (Constraint.Variable "y") (Constraint.Variable "x")
        , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8)
        ]
        [ ("x", Constraint.range 2 7)
        , ("y", Constraint.range 1 6)
        ]
    ]


constrainTest : String -> List (Constraint.Constraint String) -> List (String, Constraint.Range) -> ElmTest.Test
constrainTest name constraints expected =
  let
    initialState =
      Constraint.initialize <| List.map (\var -> (var, Constraint.range 1 10)) ["x", "y", "z"]
    state =
      List.foldl Constraint.constrain initialState constraints
    test (var, range) =
      ElmTest.test var <| ElmTest.assertEqual range (Constraint.possibleValues var state)
    tests =
      List.map test expected
  in
    ElmTest.suite name tests
