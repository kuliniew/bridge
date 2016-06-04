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
        "x > 5"
        [ Constraint.GreaterThan (Constraint.Variable "x") (Constraint.Constant 5) ]
        [ ("x", Constraint.range 6 10) ]

    , constrainTest
        "x <= 5"
        [ Constraint.Maximum (Constraint.Variable "x") (Constraint.Constant 5) ]
        [ ("x", Constraint.range 1 5) ]

    , constrainTest
        "x >= 5"
        [ Constraint.Minimum (Constraint.Variable "x") (Constraint.Constant 5) ]
        [ ("x", Constraint.range 5 10) ]

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
        "(3 < x) && (x < 7)"
        [ Constraint.And
            [ Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x")
            , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 7)
            ]
        ]
        [ ("x", Set.fromList [4, 5, 6]) ]

    , constrainTest
        "(x == 3) || (6 < x && x < 9)"
        [ Constraint.Or
            [ Constraint.Equal (Constraint.Variable "x") (Constraint.Constant 3)
            , Constraint.And
                [ Constraint.LessThan (Constraint.Constant 6) (Constraint.Variable "x")
                , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 9)
                ]
            ]
        ]
        [ ("x", Set.fromList [3, 7, 8]) ]

    , constrainTest
        "!((3 < x) && (x < 7))"
        [ Constraint.Not <|
            Constraint.And
              [ Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x")
              , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 7)
              ]
        ]
        [ ("x", Set.fromList [1, 2, 3, 7, 8, 9, 10]) ]

    , constrainTest
        "Permutation [x, y, z] [1, 3, 5]"
        [ Constraint.Permutation
            (List.map Constraint.Variable ["x", "y", "z"])
            (List.map Constraint.Constant [1, 3, 5])
        ]
        ( List.map (\var -> (var, Set.fromList [1, 3, 5])) ["x", "y", "z"] )

    , constrainTest
        "x == 3"
        [ Constraint.Equal (Constraint.Variable "x") (Constraint.Constant 3) ]
        [ ("x", Set.singleton 3) ]

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

    , constrainTest
        "true"
        [ Constraint.Null ]
        [ ("x", Constraint.range 1 10) ]
    ]


constrainTest : String -> List (Constraint.Constraint String) -> List (String, Constraint.Range) -> ElmTest.Test
constrainTest name constraints expected =
  let
    initialState =
      Constraint.initialize <| List.map (\var -> (var, Constraint.range 1 10)) ["x", "y", "z"]
    state =
      List.foldl Constraint.constrain initialState constraints
    test (var, range) =
      ElmTest.test var <| ElmTest.assertEqual (Set.toList range) (Set.toList <| Constraint.possibleValues var state)
    tests =
      List.map test expected
  in
    ElmTest.suite name tests
