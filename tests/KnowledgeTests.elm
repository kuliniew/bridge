module KnowledgeTests exposing (all)

import Card
import Knowledge
import Seat
import Solver

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Knowledge"
    [ addHandSuite
    ]


addHandSuite : ElmTest.Test
addHandSuite =
  let
    hand =
      { spades = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.King ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
      }
    knowledge =
      Knowledge.create (Card.fromSuits hand)
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
    -- TODO: lots more test cases for these
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
