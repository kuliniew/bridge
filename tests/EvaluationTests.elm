module EvaluationTests (all) where

import Card
import Evaluation

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Evaluation"
    [ highCardPointsSuite
    , distributionSuite
    , balancedSuite
    ]


highCardPointsSuite : ElmTest.Test
highCardPointsSuite =
  ElmTest.suite "highCardPoints"
    [ ElmTest.test "maximal hand" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Jack ]
            , hearts = [ Card.Ace, Card.King, Card.Queen ]
            , diamonds = [ Card.Ace, Card.King, Card.Queen ]
            , clubs = [ Card.Ace, Card.King, Card.Queen ]
            }
        in
          ElmTest.assertEqual 37 (Evaluation.highCardPoints (Card.fromSuits hand))

    , ElmTest.test "openable hand" <|
        let
          hand =
            { spades = [ Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
            , hearts = [ Card.Ten, Card.Two ]
            , diamonds = [ Card.Ace, Card.Nine ]
            , clubs = [ Card.Ace, Card.King, Card.Eight, Card.Six, Card.Five ]
            }
        in
          ElmTest.assertEqual 12 (Evaluation.highCardPoints (Card.fromSuits hand))

    , ElmTest.test "non-openable hand" <|
        let
          hand =
            { spades = [ Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven ]
            , hearts = [ Card.Ace, Card.Four, Card.Two ]
            , diamonds = [ Card.Ace, Card.Nine, Card.Two ]
            , clubs = [ Card.Seven, Card.Six ]
            }
        in
          ElmTest.assertEqual 11 (Evaluation.highCardPoints (Card.fromSuits hand))
    ]


distributionSuite : ElmTest.Test
distributionSuite =
  ElmTest.suite "distribution"
    [ ElmTest.test "measures the length of each suit regardless of order" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen ]
            , hearts = [ Card.Ace, Card.King, Card.Queen, Card.Jack ]
            , diamonds = [ Card.Ace, Card.King ]
            , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Jack ]
            }
        in
          ElmTest.assertEqual [4, 4, 3, 2] (Evaluation.distribution (Card.fromSuits hand))
    ]


balancedSuite : ElmTest.Test
balancedSuite =
  ElmTest.suite "balanced"
    [ ElmTest.test "4-4-3-2 is balanced" <|
        ElmTest.assert (Evaluation.balanced [4, 4, 3, 2])

    , ElmTest.test "4-3-3-3 is balanced" <|
        ElmTest.assert (Evaluation.balanced [4, 3, 3, 3])

    , ElmTest.test "5-3-3-2 is balanced" <|
        ElmTest.assert (Evaluation.balanced [5, 3, 3, 2])

    , ElmTest.test "5-4-2-2 is not balanced" <|
        ElmTest.assert (not <| Evaluation.balanced [5, 4, 2, 2])

    , ElmTest.test "4-4-4-1 is not balanced" <|
        ElmTest.assert (not <| Evaluation.balanced [4, 4, 4, 1])
    ]
