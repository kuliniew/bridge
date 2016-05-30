module ConstraintTests exposing (all)

import Constraint

import ElmTest
import Set exposing (Set)


all : ElmTest.Test
all =
  ElmTest.suite "Constraint"
    [ possibleValuesSuite
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
