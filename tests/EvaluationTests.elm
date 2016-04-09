module EvaluationTests (all) where

import Card
import Evaluation

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Evaluation"
    [ highCardPointsSuite
    , lengthPointsSuite
    , shortnessPointsSuite
    , pointsSuite
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


lengthPointsSuite : ElmTest.Test
lengthPointsSuite =
  ElmTest.suite "lengthPoints"
    [ ElmTest.test "no suits longer than 4" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , diamonds = [ Card.Two, Card.Three, Card.Four ]
            , clubs = [ Card.Two, Card.Three ]
            }
        in
          ElmTest.assertEqual 0 (Evaluation.lengthPoints hand)

    , ElmTest.test "one suit of length 5" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , diamonds = [ Card.Two, Card.Three ]
            , clubs = [ Card.Two, Card.Three ]
            }
        in
          ElmTest.assertEqual 1 (Evaluation.lengthPoints hand)

    , ElmTest.test "two long suits" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
            , diamonds = [ Card.Two, Card.Three ]
            , clubs = []
            }
        in
          ElmTest.assertEqual 3 (Evaluation.lengthPoints hand)

    , ElmTest.test "one suit of length 13" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven, Card.Eight, Card.Nine, Card.Ten, Card.Jack, Card.Queen, Card.King, Card.Ace ]
            , hearts = []
            , diamonds = []
            , clubs = []
            }
        in
          ElmTest.assertEqual 9 (Evaluation.lengthPoints hand)
    ]


shortnessPointsSuite : ElmTest.Test
shortnessPointsSuite =
  ElmTest.suite "shortnessPoints"
    [ ElmTest.test "no short suits" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , hearts = [ Card.Two, Card.Three, Card.Four ]
            , diamonds = [ Card.Two, Card.Three, Card.Four ]
            , clubs = [ Card.Two, Card.Three, Card.Four ]
            }
        in
          ElmTest.assertEqual 0 (Evaluation.shortnessPoints Card.Spades hand)

    , ElmTest.test "one doubleton" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , diamonds = [ Card.Two, Card.Three, Card.Four ]
            , clubs = [ Card.Two, Card.Three ]
            }
        in
          ElmTest.assertEqual 1 (Evaluation.shortnessPoints Card.Spades hand)

    , ElmTest.test "one singleton" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , clubs = [ Card.Two ]
            }
        in
          ElmTest.assertEqual 2 (Evaluation.shortnessPoints Card.Spades hand)

    , ElmTest.test "one void" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , clubs = []
            }
        in
          ElmTest.assertEqual 3 (Evaluation.shortnessPoints Card.Spades hand)

    , ElmTest.test "multiple short suits" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven ]
            , diamonds = [ Card.Two ]
            , clubs = []
            }
        in
          ElmTest.assertEqual 5 (Evaluation.shortnessPoints Card.Spades hand)

    , ElmTest.test "one suit of length 13" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven, Card.Eight, Card.Nine, Card.Ten, Card.Jack, Card.Queen, Card.King, Card.Ace ]
            , hearts = []
            , diamonds = []
            , clubs = []
            }
        in
          ElmTest.assertEqual 9 (Evaluation.shortnessPoints Card.Spades hand)

    , ElmTest.test "shortness in trump suit is ignored" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Two ]
            , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            , clubs = [ Card.Two, Card.Three, Card.Four, Card.Five ]
            }
        in
          ElmTest.assertEqual 0 (Evaluation.shortnessPoints Card.Spades hand)
    ]


pointsSuite : ElmTest.Test
pointsSuite =
  let
    hand = Card.fromSuits
      { spades = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
      , diamonds = [ Card.King, Card.Ten, Card.Nine ]
      , clubs = [ Card.Two ]
      }
    hcp = 10
    length = 1
    shortness = 2
  in
    ElmTest.suite "pointsSuite"
      [ ElmTest.test "before trumps are known, HCP and length points are counted" <|
          ElmTest.assertEqual (hcp + length) (Evaluation.points Nothing hand)

      , ElmTest.test "in a no-trump contract, only HCP are counted" <|
          ElmTest.assertEqual hcp (Evaluation.points (Just Nothing) hand)

      , ElmTest.test "in a trump contract, HCP and shortness points are counted" <|
          ElmTest.assertEqual (hcp + shortness) (Evaluation.points (Just (Just Card.Spades)) hand)
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
