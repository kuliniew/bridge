module KnowledgeTests exposing (all)

import Card
import Card.Producer
import Knowledge
import Seat
import Solver
import TestUtils

import Check
import Check.Producer
import ElmTest
import Maybe.Extra


all : ElmTest.Test
all =
  ElmTest.suite "Knowledge"
    [ addHandSuite

    , highCardPointsSuite
    , lengthPointsSuite
    , shortnessPointsSuite
    , pointsSuite

    , lengthSuite
    , distributionSuite
    , balancedSuite
    , semiBalancedSuite

    , countRankSuite

    -- , playingTricksSuite   -- FIXME: IMPLEMENT THIS

    , quickLosersSuite
    ]


addHandSuite : ElmTest.Test
addHandSuite =
  let
    knowledge =
      handKnowledge
        { spades = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
        , hearts = [ Card.King ]
        , diamonds = [ Card.Queen, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
        , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
        }
    testMetric metric expectedSelf expectedOther =
      ElmTest.suite (toString metric)
        [ ElmTest.test "self" <|
            ElmTest.assertEqual expectedSelf (Knowledge.get Seat.Self metric knowledge)
        , ElmTest.test "left opponent" <|
            ElmTest.assertEqual expectedOther (Knowledge.get Seat.LeftOpponent metric knowledge)
        , ElmTest.test "partner" <|
            ElmTest.assertEqual expectedOther (Knowledge.get Seat.Partner metric knowledge)
        , ElmTest.test "right opponent" <|
            ElmTest.assertEqual expectedOther (Knowledge.get Seat.RightOpponent metric knowledge)
        ]
  in
    ElmTest.suite "addHand"
      [ testMetric
          Knowledge.HighCardPoints
          (Solver.singleton 13)
          (Solver.range 0 27)

      , testMetric
          Knowledge.LengthPoints
          (Solver.singleton 1)
          (Solver.range 0 9)

      , testMetric
          (Knowledge.LengthPointsWithinSuit Card.Spades)
          (Solver.singleton 0)
          (Solver.range 0 5)

      , testMetric
          (Knowledge.LengthPointsWithinSuit Card.Hearts)
          (Solver.singleton 0)
          (Solver.range 0 8)

      , testMetric
          (Knowledge.LengthPointsWithinSuit Card.Diamonds)
          (Solver.singleton 1)
          (Solver.range 0 4)

      , testMetric
          (Knowledge.LengthPointsWithinSuit Card.Clubs)
          (Solver.singleton 0)
          (Solver.range 0 6)

      , testMetric
          Knowledge.UncommittedPoints
          (Solver.singleton 14)
          (Solver.range 0 36)

      , testMetric
          (Knowledge.ShortnessPointsWithTrump Card.Spades)
          (Solver.singleton 2)
          (Solver.range 0 9)

      , testMetric
          (Knowledge.ShortnessPointsWithTrump Card.Hearts)
          (Solver.singleton 0)
          (Solver.range 0 9)

      , testMetric
          (Knowledge.ShortnessPointsWithTrump Card.Diamonds)
          (Solver.singleton 2)
          (Solver.range 0 9)

      , testMetric
          (Knowledge.ShortnessPointsWithTrump Card.Clubs)
          (Solver.singleton 2)
          (Solver.range 0 9)

      , testMetric
          (Knowledge.ShortnessPointsWithinSuit Card.Spades)
          (Solver.singleton 0)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.ShortnessPointsWithinSuit Card.Hearts)
          (Solver.singleton 2)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.ShortnessPointsWithinSuit Card.Diamonds)
          (Solver.singleton 0)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.ShortnessPointsWithinSuit Card.Clubs)
          (Solver.singleton 0)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.PointsWithTrump Card.Spades)
          (Solver.singleton 15)
          (Solver.range 0 36)

      , testMetric
          (Knowledge.PointsWithTrump Card.Hearts)
          (Solver.singleton 13)
          (Solver.range 0 36)

      , testMetric
          (Knowledge.PointsWithTrump Card.Diamonds)
          (Solver.singleton 15)
          (Solver.range 0 36)

      , testMetric
          (Knowledge.PointsWithTrump Card.Clubs)
          (Solver.singleton 15)
          (Solver.range 0 36)

      , testMetric
          (Knowledge.Length Card.Spades)
          (Solver.singleton 4)
          (Solver.range 0 9)

      , testMetric
          (Knowledge.Length Card.Hearts)
          (Solver.singleton 1)
          (Solver.range 0 12)

      , testMetric
          (Knowledge.Length Card.Diamonds)
          (Solver.singleton 5)
          (Solver.range 0 8)

      , testMetric
          (Knowledge.Length Card.Clubs)
          (Solver.singleton 3)
          (Solver.range 0 10)

      , testMetric
          (Knowledge.CountRank Card.Ace)
          (Solver.singleton 2)
          (Solver.range 0 2)

      , testMetric
          (Knowledge.CountRank Card.King)
          (Solver.singleton 1)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.CountRank Card.Queen)
          (Solver.singleton 1)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.CountRank Card.Jack)
          (Solver.singleton 0)
          (Solver.range 0 4)

      , testMetric
          (Knowledge.CountRank Card.Ten)
          (Solver.singleton 3)
          (Solver.range 0 1)

      , testMetric
          (Knowledge.CountRank Card.Nine)
          (Solver.singleton 3)
          (Solver.range 0 1)

      , testMetric
          (Knowledge.CountRank Card.Eight)
          (Solver.singleton 2)
          (Solver.range 0 2)

      , testMetric
          (Knowledge.CountRank Card.Seven)
          (Solver.singleton 1)
          (Solver.range 0 3)

      , testMetric
          (Knowledge.CountRank Card.Six)
          (Solver.singleton 0)
          (Solver.range 0 4)

      , testMetric
          (Knowledge.CountRank Card.Five)
          (Solver.singleton 0)
          (Solver.range 0 4)

      , testMetric
          (Knowledge.CountRank Card.Four)
          (Solver.singleton 0)
          (Solver.range 0 4)

      , testMetric
          (Knowledge.CountRank Card.Three)
          (Solver.singleton 0)
          (Solver.range 0 4)

      , testMetric
          (Knowledge.CountRank Card.Two)
          (Solver.singleton 0)
          (Solver.range 0 4)

      -- TODO: PLAYING TRICKS
      -- TODO: QUICK LOSERS
      ]


highCardPointsSuite : ElmTest.Test
highCardPointsSuite =
  ElmTest.suite "highCardPoints"
    [ testExactMetric "maximal hand" Knowledge.HighCardPoints 37
        { spades = [ Card.Ace, Card.King, Card.Queen, Card.Jack ]
        , hearts = [ Card.Ace, Card.King, Card.Queen ]
        , diamonds = [ Card.Ace, Card.King, Card.Queen ]
        , clubs = [ Card.Ace, Card.King, Card.Queen ]
        }

    , testExactMetric "openable hand" Knowledge.HighCardPoints 12
        { spades = [ Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
        , hearts = [ Card.Ten, Card.Two ]
        , diamonds = [ Card.Ace, Card.Nine ]
        , clubs = [ Card.Ace, Card.King, Card.Eight, Card.Six, Card.Five ]
        }

    , testExactMetric "non-openable hand" Knowledge.HighCardPoints 11
        { spades = [ Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven ]
        , hearts = [ Card.Ace, Card.Four, Card.Two ]
        , diamonds = [ Card.Ace, Card.Nine, Card.Two ]
        , clubs = [ Card.Seven, Card.Six ]
        }
    ]


lengthPointsSuite : ElmTest.Test
lengthPointsSuite =
  ElmTest.suite "lengthPoints"
    [ testExactMetric "no suits longer than 4" Knowledge.LengthPoints 0
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , diamonds = [ Card.Two, Card.Three, Card.Four ]
        , clubs = [ Card.Two, Card.Three ]
        }

    , testExactMetric "one suit of length 5" Knowledge.LengthPoints 1
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , diamonds = [ Card.Two, Card.Three ]
        , clubs = [ Card.Two, Card.Three ]
        }

    , testExactMetric "two long suits" Knowledge.LengthPoints 3
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
        , diamonds = [ Card.Two, Card.Three ]
        , clubs = []
        }

    , testExactMetric "one suit of length 13" Knowledge.LengthPoints 9
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven, Card.Eight, Card.Nine, Card.Ten, Card.Jack, Card.Queen, Card.King, Card.Ace ]
        , hearts = []
        , diamonds = []
        , clubs = []
        }
    ]


shortnessPointsSuite : ElmTest.Test
shortnessPointsSuite =
  ElmTest.suite "shortnessPoints"
    [ testExactMetric "no short suits" (Knowledge.ShortnessPointsWithTrump Card.Spades) 0
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Two, Card.Three, Card.Four ]
        , clubs = [ Card.Two, Card.Three, Card.Four ]
        }

    , testExactMetric "one doubleton" (Knowledge.ShortnessPointsWithTrump Card.Spades) 1
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , diamonds = [ Card.Two, Card.Three, Card.Four ]
        , clubs = [ Card.Two, Card.Three ]
        }

    , testExactMetric "one singleton" (Knowledge.ShortnessPointsWithTrump Card.Spades) 2
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , clubs = [ Card.Two ]
        }

    , testExactMetric "one void" (Knowledge.ShortnessPointsWithTrump Card.Spades) 3
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , clubs = []
        }

    , testExactMetric "multiple short suits" (Knowledge.ShortnessPointsWithTrump Card.Spades) 5
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven ]
        , diamonds = [ Card.Two ]
        , clubs = []
        }

    , testExactMetric "one suit of length 13" (Knowledge.ShortnessPointsWithTrump Card.Spades) 9
        { spades = [ Card.Two, Card.Three, Card.Four, Card.Five, Card.Six, Card.Seven, Card.Eight, Card.Nine, Card.Ten, Card.Jack, Card.Queen, Card.King, Card.Ace ]
        , hearts = []
        , diamonds = []
        , clubs = []
        }

    , testExactMetric "shortness in trump suit is ignored" (Knowledge.ShortnessPointsWithTrump Card.Spades) 0
        { spades = [ Card.Two ]
        , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        , clubs = [ Card.Two, Card.Three, Card.Four, Card.Five ]
        }
    ]


pointsSuite : ElmTest.Test
pointsSuite =
  let
    hand =
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
      [ testExactMetric "before trumps are known, HCP and length points are counted" Knowledge.UncommittedPoints (hcp + length) hand

      , testExactMetric "in a no-trump contract, only HCP are counted" Knowledge.HighCardPoints hcp hand

      , testExactMetric "in a trump contract, HCP and shortness points are counted" (Knowledge.PointsWithTrump Card.Spades) (hcp + shortness) hand
      ]


lengthSuite : ElmTest.Test
lengthSuite =
  let
    hand =
      { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
      , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
      , diamonds = [ Card.Ace, Card.Two, Card.Three ]
      , clubs = []
      }
  in
    ElmTest.suite "lengthSuite"
      [ testExactMetric "non-void suit" (Knowledge.Length Card.Spades) 6 hand

      , testExactMetric "void suit" (Knowledge.Length Card.Clubs) 0 hand
      ]


distributionSuite : ElmTest.Test
distributionSuite =
  ElmTest.suite "distribution"
    [ testSatisfied "measures the length of each suit regardless of order" (Knowledge.distribution 4 4 3 2)
        { spades = [ Card.Ace, Card.King, Card.Queen ]
        , hearts = [ Card.Ace, Card.King, Card.Queen, Card.Jack ]
        , diamonds = [ Card.Ace, Card.King ]
        , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Jack ]
        }
    ]


balancedSuite : ElmTest.Test
balancedSuite =
  ElmTest.suite "balanced"
    [ testSatisfied "4-4-3-2 is balanced" Knowledge.balanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testSatisfied "4-3-3-3 is balanced" Knowledge.balanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , hearts = [ Card.Ace, Card.Two, Card.Three ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three ]
        , clubs = [ Card.Ace, Card.Two, Card.Three ]
        }

    , testSatisfied "5-3-3-2 is balanced" Knowledge.balanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Ace, Card.Two, Card.Three ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testNotSatisfied "5-4-2-2 is not balanced" Knowledge.balanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Ace, Card.Two ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testNotSatisfied "6-3-2-2 is not balanced" Knowledge.balanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
        , hearts = [ Card.Ace, Card.Two, Card.Three ]
        , diamonds = [ Card.Ace, Card.Two ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testNotSatisfied "4-4-4-1 is not balanced" Knowledge.balanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , clubs = [ Card.Ace ]
        }
    ]


semiBalancedSuite : ElmTest.Test
semiBalancedSuite =
  ElmTest.suite "semiBalanced"
    [ testSatisfied "5-4-2-2 is semi-balanced" Knowledge.semiBalanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Ace, Card.Two ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testSatisfied "6-3-2-2 is semi-balanced" Knowledge.semiBalanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five, Card.Six ]
        , hearts = [ Card.Ace, Card.Two, Card.Three ]
        , diamonds = [ Card.Ace, Card.Two ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testNotSatisfied "4-4-3-2 is not semi-balanced" Knowledge.semiBalanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testNotSatisfied "4-3-3-3 is not semi-balanced" Knowledge.semiBalanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , hearts = [ Card.Ace, Card.Two, Card.Three ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three ]
        , clubs = [ Card.Ace, Card.Two, Card.Three ]
        }

    , testNotSatisfied "5-3-3-2 is not semi-balanced" Knowledge.semiBalanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
        , hearts = [ Card.Ace, Card.Two, Card.Three ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three ]
        , clubs = [ Card.Ace, Card.Two ]
        }

    , testNotSatisfied "4-4-4-1 is not semi-balanced" Knowledge.semiBalanced
        { spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , diamonds = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
        , clubs = [ Card.Ace ]
        }
    ]


countRankSuite : ElmTest.Test
countRankSuite =
  let
    hand =
      { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine ]
      , hearts = [ Card.Queen, Card.Ten, Card.Nine]
      , diamonds = [ Card.Ace, Card.Ten, Card.Nine ]
      , clubs = [ Card.Ten, Card.Nine, Card.Eight ]
      }
  in
    ElmTest.suite "countRank"
      [ testExactMetric "have cards of that rank" (Knowledge.CountRank Card.Ace) 2 hand

      , testExactMetric "have no cards of that rank" (Knowledge.CountRank Card.King) 0 hand
      ]


quickLosersSuite : ElmTest.Test
quickLosersSuite =
  let
    spadesHand ranks =
      { spades = ranks
      , hearts = []
      , diamonds = []
      , clubs = List.take (13 - List.length ranks) Card.ranks
      }
    test name expected ranks =
      testExactMetric name (Knowledge.QuickLosers Card.Spades) expected (spadesHand ranks)
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
            (\(suit, hand) ->
              let
                constraint =
                  Solver.variable (Knowledge.QuickLosers suit) `Solver.greaterThan` Solver.variable (Knowledge.Length suit)
              in
                Maybe.Extra.isNothing <| Knowledge.addConstraint Seat.Self constraint <| Knowledge.create hand)
          `Check.for`
            Check.Producer.tuple (Card.Producer.suit, Card.Producer.hand)
      ]


testExactMetric : String -> Knowledge.Metric -> Int -> Card.SampleHand a -> ElmTest.Test
testExactMetric name metric expected hand =
  ElmTest.test name <|
    ElmTest.assertEqual (Solver.singleton expected) (Knowledge.get Seat.Self metric <| handKnowledge hand)


testSatisfied : String -> Solver.Constraint Knowledge.Metric -> Card.SampleHand a -> ElmTest.Test
testSatisfied name constraint hand =
  ElmTest.test name <|
    ElmTest.assert <| Maybe.Extra.isJust <| Knowledge.addConstraint Seat.Self constraint <| handKnowledge hand


testNotSatisfied : String -> Solver.Constraint Knowledge.Metric -> Card.SampleHand a -> ElmTest.Test
testNotSatisfied name constraint hand =
  ElmTest.test name <|
    ElmTest.assert <| Maybe.Extra.isNothing <| Knowledge.addConstraint Seat.Self constraint <| handKnowledge hand


handKnowledge : Card.SampleHand a -> Knowledge.Knowledge
handKnowledge =
  Knowledge.create << Card.fromSuits
