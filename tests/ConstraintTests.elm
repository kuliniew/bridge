module ConstraintTests exposing (all)

import Constraint

import ElmTest
import Set exposing (Set)
import String


all : ElmTest.Test
all =
  ElmTest.suite "Constraint"
    [ possibleValuesSuite
    , rangeSuite
    , constrainSuite
    , guaranteedSuite
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
    [ standardConstrainTest
        "x < 5"
        [ Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 5) ]
        [  ("x", Constraint.range 1 4) ]

    , standardConstrainTest
        "5 < x"
        [ Constraint.LessThan (Constraint.Constant 5) (Constraint.Variable "x") ]
        [ ("x", Constraint.range 6 10) ]

    , standardConstrainTest
        "x > 5"
        [ Constraint.GreaterThan (Constraint.Variable "x") (Constraint.Constant 5) ]
        [ ("x", Constraint.range 6 10) ]

    , standardConstrainTest
        "x <= 5"
        [ Constraint.Maximum (Constraint.Variable "x") (Constraint.Constant 5) ]
        [ ("x", Constraint.range 1 5) ]

    , standardConstrainTest
        "x >= 5"
        [ Constraint.Minimum (Constraint.Variable "x") (Constraint.Constant 5) ]
        [ ("x", Constraint.range 5 10) ]

    , standardConstrainTest
        "x + y + z < 6"
        [ Constraint.LessThan
            (Constraint.Add <| List.map Constraint.Variable ["x", "y", "z"])
            (Constraint.Constant 6)
        ]
        ( List.map (\var -> (var, Constraint.range 1 3)) ["x", "y", "z"] )

    , standardConstrainTest
        "x > 1 ; y > 1 ; x * y < 10"
        [ Constraint.GreaterThan (Constraint.Variable "x") (Constraint.Constant 1)
        , Constraint.GreaterThan (Constraint.Variable "y") (Constraint.Constant 1)
        , Constraint.LessThan
            (Constraint.Multiply <| List.map Constraint.Variable ["x", "y"])
            (Constraint.Constant 10)
        ]
        ( List.map (\var -> (var, Constraint.range 2 4)) ["x", "y"] )

    , standardConstrainTest
        "(x < 3) || (7 < x)"
        [ Constraint.Or
            [ Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 3)
            , Constraint.LessThan (Constraint.Constant 7) (Constraint.Variable "x")
            ]
        ]
        [ ("x", Set.fromList [1, 2, 8, 9, 10]) ]

    , standardConstrainTest
        "(3 < x) && (x < 7)"
        [ Constraint.And
            [ Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x")
            , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 7)
            ]
        ]
        [ ("x", Set.fromList [4, 5, 6]) ]

    , standardConstrainTest
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

    , standardConstrainTest
        "!((3 < x) && (x < 7))"
        [ Constraint.Not <|
            Constraint.And
              [ Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x")
              , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 7)
              ]
        ]
        [ ("x", Set.fromList [1, 2, 3, 7, 8, 9, 10]) ]

    , standardConstrainTest
        "Permutation [x, y, z] [1, 3, 5]"
        [ Constraint.Permutation
            (List.map Constraint.Variable ["x", "y", "z"])
            (List.map Constraint.Constant [1, 3, 5])
        ]
        ( List.map (\var -> (var, Set.fromList [1, 3, 5])) ["x", "y", "z"] )

    , standardConstrainTest
        "x == 3"
        [ Constraint.Equal (Constraint.Variable "x") (Constraint.Constant 3) ]
        [ ("x", Set.singleton 3) ]

    , standardConstrainTest
        "x == 5 + (-y)"
        [ Constraint.Equal
            (Constraint.Variable "x")
            (Constraint.Add [Constraint.Constant 5, Constraint.Negate <| Constraint.Variable "y"] )
        ]
        [ ("x", Constraint.range 1 4)
        , ("y", Constraint.range 1 4)
        ]

    , standardConstrainTest
        "x = 3 ; y = 5 ; z = max(x,y)"
        [ Constraint.Equal (Constraint.Variable "x") (Constraint.Constant 3)
        , Constraint.Equal (Constraint.Variable "y") (Constraint.Constant 5)
        , Constraint.Equal (Constraint.Variable "z") (Constraint.Max <| List.map Constraint.Variable ["x", "y"])
        ]
        [ ("z", Set.singleton 5) ]

    , standardConstrainTest
        "missing"
        [ Constraint.LessThan (Constraint.Variable "missing") (Constraint.Constant 5) ]
        [ ("missing", Set.empty) ]

    , standardConstrainTest
        "(3 < x) ; (x < 8)"
        [ Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x")
        , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8)
        ]
        [ ("x", Constraint.range 4 7) ]

    , standardConstrainTest
        "(x < 8) ; (y < x)"
        [ Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8)
        , Constraint.LessThan (Constraint.Variable "y") (Constraint.Variable "x")
        ]
        [ ("x", Constraint.range 2 7)
        , ("y", Constraint.range 1 6)
        ]

    , standardConstrainTest
        "(y < x) ; (x < 8)"
        [ Constraint.LessThan (Constraint.Variable "y") (Constraint.Variable "x")
        , Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8)
        ]
        [ ("x", Constraint.range 2 7)
        , ("y", Constraint.range 1 6)
        ]

    , standardConstrainTest
        "true"
        [ Constraint.Null ]
        [ ("x", Constraint.range 1 10) ]

    , largeConstrainTest
        "100 = sum(a .. z)"
        [ Constraint.Equal
            (Constraint.Constant 100)
            (Constraint.Add largeConstrainTestVars)
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "sum(a .. z) = 100"
        [ Constraint.Equal
            (Constraint.Add largeConstrainTestVars)
            (Constraint.Constant 100)
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "(100 = sum(a .. z)) ; (a = 15)"
        [ Constraint.Equal
            (Constraint.Constant 100)
            (Constraint.Add largeConstrainTestVars)
        , Constraint.Equal
            (Constraint.Variable "a")
            (Constraint.Constant 15)
        ]
        (("a", Set.singleton 15) :: List.map (\var -> (var, Constraint.range 1 61)) (Maybe.withDefault [] <| List.tail largeConstrainTestVarNames))

    , largeConstrainTest
        "2590 = sum(a .. z)"
        [ Constraint.Equal
            (Constraint.Constant 2590)
            (Constraint.Add largeConstrainTestVars)
        ]
        (List.map (\var -> (var, Constraint.range 90 100)) largeConstrainTestVarNames)

    , largeConstrainTest
        "sum(a .. z) = 2590"
        [ Constraint.Equal
            (Constraint.Add largeConstrainTestVars)
            (Constraint.Constant 2590)
        ]
        (List.map (\var -> (var, Constraint.range 90 100)) largeConstrainTestVarNames)

    , largeConstrainTest
        "-100 = sum(-a .. -z)"
        [ Constraint.Equal
            (Constraint.Constant (-100))
            (Constraint.Add <| List.map Constraint.Negate largeConstrainTestVars)
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "sum(-a .. -z) = -100"
        [ Constraint.Equal
            (Constraint.Add <| List.map Constraint.Negate largeConstrainTestVars)
            (Constraint.Constant (-100))
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "100000 = sum(1000 * a .. 1000 * z)"
        [ Constraint.Equal
            (Constraint.Constant 100000)
            (Constraint.Add <| List.map (\var -> Constraint.Multiply [Constraint.Constant 1000, var]) largeConstrainTestVars)
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "sum(1000 * a .. 1000 * z) = 100000"
        [ Constraint.Equal
            (Constraint.Add <| List.map (\var -> Constraint.Multiply [Constraint.Constant 1000, var]) largeConstrainTestVars)
            (Constraint.Constant 100000)
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "-100 = sum(-1 * a .. -1 * z)"
        [ Constraint.Equal
            (Constraint.Constant (-100))
            (Constraint.Add <| List.map (\var -> Constraint.Multiply [Constraint.Constant (-1), var]) largeConstrainTestVars)
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)

    , largeConstrainTest
        "sum(-1 * a .. -1 * z) = -100"
        [ Constraint.Equal
            (Constraint.Add <| List.map (\var -> Constraint.Multiply [Constraint.Constant (-1), var]) largeConstrainTestVars)
            (Constraint.Constant (-100))
        ]
        (List.map (\var -> (var, Constraint.range 1 75)) largeConstrainTestVarNames)
    ]


standardConstrainTest : String -> List (Constraint.Constraint String) -> List (String, Constraint.Range) -> ElmTest.Test
standardConstrainTest = constrainTest "xyz" (Constraint.range 1 10)


largeConstrainTest : String -> List (Constraint.Constraint String) -> List (String, Constraint.Range) -> ElmTest.Test
largeConstrainTest = constrainTest (String.join "" largeConstrainTestVarNames) (Constraint.range 1 100)


largeConstrainTestVars : List (Constraint.Term String)
largeConstrainTestVars = List.map Constraint.Variable largeConstrainTestVarNames


largeConstrainTestVarNames : List String
largeConstrainTestVarNames = String.split "" "abcdefghijklmnopqrstuvwxyz"


constrainTest : String -> Constraint.Range -> String -> List (Constraint.Constraint String) -> List (String, Constraint.Range) -> ElmTest.Test
constrainTest vars initialRange name constraints expected =
  let
    initialState =
      Constraint.initialize <| List.map (\var -> (var, initialRange)) (String.split "" vars)
    state =
      List.foldl Constraint.constrain initialState constraints
    test (var, range) =
      ElmTest.test var <| ElmTest.assertEqual (Set.toList range) (Set.toList <| Constraint.possibleValues var state)
    tests =
      List.map test expected
  in
    ElmTest.suite name tests


guaranteedSuite : ElmTest.Test
guaranteedSuite =
  let
    state =
      Constraint.initialize [ ("x", Constraint.range 3 7) ]
    greaterThan val =
      Constraint.GreaterThan (Constraint.Variable "x") (Constraint.Constant val)
    exactConstraint =
      Constraint.And
        [ Constraint.Minimum (Constraint.Variable "x") (Constraint.Constant 3)
        , Constraint.Maximum (Constraint.Variable "x") (Constraint.Constant 7)
        ]
  in
    ElmTest.suite "guaranteed"
      [ ElmTest.test "not even possible" <|
          ElmTest.assert <| not <| Constraint.guaranteed (greaterThan 8) state
      , ElmTest.test "possible, but not guaranteed" <|
          ElmTest.assert <| not <| Constraint.guaranteed (greaterThan 5) state
      , ElmTest.test "guaranteed exactly" <|
          ElmTest.assert <| Constraint.guaranteed exactConstraint state
      , ElmTest.test "guaranteed and overconstrained" <|
          ElmTest.assert <| Constraint.guaranteed (greaterThan 2) state
      ]
