module ConstraintTests exposing (all)

import Constraint

import ElmTest
import Set exposing (Set)


all : ElmTest.Test
all =
  ElmTest.suite "Constraint"
    [ possibleValuesSuite
    , rangeSuite
    , singleConstrainSuite
    , multipleConstrainSuite
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


singleConstrainSuite : ElmTest.Test
singleConstrainSuite =
  let
    initialState =
      Constraint.initialize <| List.map (\var -> (var, Constraint.range 1 10)) ["x", "y", "z"]
  in
    ElmTest.suite "constrain (single)"
      [ ElmTest.test "LessThan x 5" <|
          let
            state =
              Constraint.constrain (Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 5)) initialState
          in
            ElmTest.assertEqual (Constraint.range 1 4) (Constraint.possibleValues "x" state)

      , ElmTest.test "LessThan 5 x" <|
          let
            state =
              Constraint.constrain (Constraint.LessThan (Constraint.Constant 5) (Constraint.Variable "x")) initialState
          in
            ElmTest.assertEqual (Constraint.range 6 10) (Constraint.possibleValues "x" state)

      , ElmTest.suite "LessThan (Sum x y z) 6" <|
          let
            variableSum =
              Constraint.Add <| List.map Constraint.Variable ["x", "y", "z"]
            state =
              Constraint.constrain (Constraint.LessThan variableSum (Constraint.Constant 6)) initialState
            test var =
              ElmTest.test var <| ElmTest.assertEqual (Constraint.range 1 3) (Constraint.possibleValues var state)
          in
            List.map test ["x", "y", "z"]

      , ElmTest.test "does not introduce new variables" <|
          let
            state =
              Constraint.constrain (Constraint.LessThan (Constraint.Variable "missing") (Constraint.Constant 5)) initialState
          in
            ElmTest.assertEqual Set.empty (Constraint.possibleValues "missing" state)
      ]


multipleConstrainSuite : ElmTest.Test
multipleConstrainSuite =
  let
    initialState =
      Constraint.initialize
        [ ("x", Constraint.range 1 10)
        , ("y", Constraint.range 1 10)
        ]
  in
    ElmTest.suite "constrain (multiple)"
      [ ElmTest.test "over a single variable" <|
          let
            state =
              initialState
                |> Constraint.constrain (Constraint.LessThan (Constraint.Constant 3) (Constraint.Variable "x"))
                |> Constraint.constrain (Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8))
          in
            ElmTest.assertEqual (Constraint.range 4 7) (Constraint.possibleValues "x" state)

      , ElmTest.test "over two variables, no re-evaluation needed" <|
          let
            state =
              initialState
                |> Constraint.constrain (Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8))
                |> Constraint.constrain (Constraint.LessThan (Constraint.Variable "y") (Constraint.Variable "x"))
          in
            ElmTest.assertEqual (Constraint.range 1 6) (Constraint.possibleValues "y" state)

      , ElmTest.test "over two variables, re-evaluation needed" <|
          let
            state =
              initialState
                |> Constraint.constrain (Constraint.LessThan (Constraint.Variable "y") (Constraint.Variable "x"))
                |> Constraint.constrain (Constraint.LessThan (Constraint.Variable "x") (Constraint.Constant 8))
          in
            ElmTest.assertEqual (Constraint.range 1 6) (Constraint.possibleValues "y" state)
      ]
