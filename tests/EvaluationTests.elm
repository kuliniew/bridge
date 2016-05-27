module EvaluationTests exposing (all)

import Card
import Card.Producer
import Evaluation
import TestUtils

import Check
import Check.Producer
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Evaluation"
    [ highCardPointsSuite
    , lengthPointsSuite
    , shortnessPointsSuite
    , pointsSuite

    , lengthSuite
    , distributionSuite
    , balancedSuite
    , semiBalancedSuite

    , playingTricksSuite

    , quickLosersSuite
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


lengthSuite : ElmTest.Test
lengthSuite =
  let
    hand = Card.fromSuits
      { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
      , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
      , diamonds = [ Card.Ace, Card.Two, Card.Three ]
      , clubs = []
      }
  in
    ElmTest.suite "lengthSuite"
      [ ElmTest.test "non-void suit" <|
          ElmTest.assertEqual 6 (Evaluation.length Card.Spades hand)

      , ElmTest.test "void suit" <|
          ElmTest.assertEqual 0 (Evaluation.length Card.Clubs hand)
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

    , ElmTest.test "6-3-2-2 is not balanced" <|
        ElmTest.assert (not <| Evaluation.balanced [6, 3, 2, 2])

    , ElmTest.test "4-4-4-1 is not balanced" <|
        ElmTest.assert (not <| Evaluation.balanced [4, 4, 4, 1])
    ]


semiBalancedSuite : ElmTest.Test
semiBalancedSuite =
  ElmTest.suite "semiBalanced"
    [ ElmTest.test "5-4-2-2 is semi-balanced" <|
        ElmTest.assert (Evaluation.semiBalanced [5, 4, 2, 2])

    , ElmTest.test "6-3-2-2 is semi-balanced" <|
        ElmTest.assert (Evaluation.semiBalanced [6, 3, 2, 2])

    , ElmTest.test "4-4-3-2 is not semi-balanced" <|
        ElmTest.assert (not <| Evaluation.semiBalanced [4, 4, 3, 2])

    , ElmTest.test "4-3-3-3 is not semi-balanced" <|
        ElmTest.assert (not <| Evaluation.semiBalanced [4, 3, 3, 3])

    , ElmTest.test "5-3-3-2 is not semi-balanced" <|
        ElmTest.assert (not <| Evaluation.semiBalanced [5, 3, 3, 2])

    , ElmTest.test "4-4-4-1 is not semi-balanced" <|
        ElmTest.assert (not <| Evaluation.semiBalanced [4, 4, 4, 1])
    ]


playingTricksSuite : ElmTest.Test
playingTricksSuite =
  ElmTest.suite "playingTricks"
    [ ElmTest.test "3 spades, 2 hearts, 1 diamond, with no trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Nine, Card.Eight, Card.Seven ]
            , hearts = [ Card.Ace, Card.King, Card.Six ]
            , diamonds = [ Card.Ace, Card.Three, Card.Two ]
            , clubs = [ Card.Five ]
            }
        in
          ElmTest.assertEqual 6 (Evaluation.playingTricks Nothing hand)

    , ElmTest.test "6 spades, 2 hearts, 1 diamond, with spades as trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Nine, Card.Eight, Card.Seven ]
            , hearts = [ Card.Ace, Card.King, Card.Six ]
            , diamonds = [ Card.Ace, Card.Three, Card.Two ]
            , clubs = [ Card.Five ]
            }
        in
          ElmTest.assertEqual 9 (Evaluation.playingTricks (Just Card.Spades) hand)

    , ElmTest.test "5 clubs, 3 hearts, 2 diamonds, with clubs as trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Five ]
            , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten ]
            , diamonds = [ Card.Ace, Card.King ]
            , clubs = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
            }
        in
          ElmTest.assertEqual 10 (Evaluation.playingTricks (Just Card.Clubs) hand)

    , ElmTest.test "7 spades, 2 hearts, with spades as trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven ]
            , hearts = [ Card.Ace, Card.King, Card.Ten ]
            , diamonds = [ Card.Six, Card.Five, Card.Three ]
            , clubs = []
            }
        in
          ElmTest.assertEqual 9 (Evaluation.playingTricks (Just Card.Spades) hand)

    , ElmTest.test "6 spades, with spades as trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven, Card.Five ]
            , hearts = [ Card.Six, Card.Two ]
            , diamonds = [ Card.Nine, Card.Five ]
            , clubs = [ Card.Eight, Card.Three ]
            }
        in
          ElmTest.assertEqual 6 (Evaluation.playingTricks (Just Card.Spades) hand)

    , ElmTest.test "5 diamonds, 1 spade, with diamonds as trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Ace, Card.Four ]
            , hearts = [ Card.Nine, Card.Five, Card.Two ]
            , diamonds = [ Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven, Card.Three, Card.Two ]
            , clubs = [ Card.Seven ]
            }
        in
          ElmTest.assertEqual 6 (Evaluation.playingTricks (Just Card.Diamonds) hand)

    , ElmTest.test "5 hearts, with hearts as trumps" <|
        let
          hand = Card.fromSuits
            { spades = [ Card.Jack, Card.Five ]
            , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Five, Card.Four, Card.Three ]
            , diamonds = [ Card.Seven, Card.Three ]
            , clubs = [ Card.Six, Card.Two ]
            }
        in
          ElmTest.assertEqual 5 (Evaluation.playingTricks (Just Card.Hearts) hand)

    , ElmTest.test "8 clubs, with clubs as trumps" <|
        let
          hand = Card.fromSuits
            { spades = []
            , hearts = [ Card.Five, Card.Four, Card.Two ]
            , diamonds = [ Card.Jack, Card.Eight ]
            , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Seven, Card.Six, Card.Three ]
            }
        in
          ElmTest.assertEqual 8 (Evaluation.playingTricks (Just Card.Clubs) hand)

    , ElmTest.test "3 clubs, with spades as trumps" <|
        let
          hand = Card.fromSuits
            { spades = []
            , hearts = [ Card.Five, Card.Four, Card.Two ]
            , diamonds = [ Card.Jack, Card.Eight ]
            , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Seven, Card.Six, Card.Three ]
            }
        in
          ElmTest.assertEqual 3 (Evaluation.playingTricks (Just Card.Spades) hand)
    ]


quickLosersSuite : ElmTest.Test
quickLosersSuite =
  let
    spadesHand ranks = Card.fromSuits
      { spades = ranks
      , hearts = []
      , diamonds = []
      , clubs = List.take (13 - List.length ranks) Card.ranks
      }
    test name expected ranks =
      ElmTest.test name <| ElmTest.assertEqual expected (Evaluation.quickLosers Card.Spades <| spadesHand ranks)
  in
    ElmTest.suite "quickLosers"
      [ test "void suit" 0 []

      , test "singleton ace" 0 [ Card.Ace ]
      , test "doubleton ace-king" 0 [ Card.Ace, Card.King ]
      , test "doubleton ace-queen" 0 [ Card.Ace, Card.Queen ]
      , test "doubleton ace-spot" 0 [ Card.Ace, Card.Ten ]

      , test "singleton king" 1 [ Card.King ]
      , test "doubleton king-queen" 1 [ Card.King, Card.Queen ]
      , test "doubleton king-spot" 1 [ Card.King, Card.Ten ]

      , test "singleton queen" 1 [ Card.Queen ]
      , test "doubleton queen-jack" 2 [ Card.Queen, Card.Jack ]
      , test "three-length queen-jack-spot" 2 [ Card.Queen, Card.Jack, Card.Ten ]

      , test "singleton jack" 1 [ Card.Jack ]
      , test "doubleton jack-spot" 2 [ Card.Jack, Card.Ten ]
      , test "three-length jack-spot-spot" 3 [ Card.Jack, Card.Ten, Card.Nine ]
      , test "four-length jack-spot-spot-spot" 3 [ Card.Jack, Card.Ten, Card.Nine, Card.Eight ]

      , test "singleton spot" 1 [ Card.Ten ]
      , test "doubleton spot-spot" 2 [ Card.Ten, Card.Nine ]
      , test "three-length spot-spot-spot" 3 [ Card.Ten, Card.Nine, Card.Eight ]
      , test "four-length spot-spot-spot-spot" 4 [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]

      , TestUtils.generativeTest <|
          Check.claim
            "is never more than the length of the suit"
          `Check.true`
            (\(suit, hand) -> Evaluation.quickLosers suit hand <= Evaluation.length suit hand)
          `Check.for`
            Check.Producer.tuple (Card.Producer.suit, Card.Producer.hand)
      ]
