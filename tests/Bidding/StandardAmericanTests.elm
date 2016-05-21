module Bidding.StandardAmericanTests (all) where

import Auction
import Bidding
import Bidding.StandardAmerican
import Bidding.TestUtils
import Card exposing (Card)
import TestUtils
import Vulnerability

import ElmTest
import List.Extra


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.StandardAmericanTests"
    [ openingSuite
    , oneNoTrumpResponseSuite
    , twoNoTrumpResponseSuite
    , threeNoTrumpResponseSuite
    , conventionResponseSuite
    ]


openingSuite : ElmTest.Test
openingSuite =
  let
    existenceTests =
      List.map (Bidding.TestUtils.suggestionsExist Bidding.StandardAmerican.system) <| List.Extra.inits (List.repeat 3 Auction.Pass)
    unitTests =
      List.map (Bidding.TestUtils.testBid Bidding.StandardAmerican.system)
        [ { name = "4 HCP, 7 hearts, 5 tricks, unfavorable vulnerability"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Unfavorable
          , history = []
          , spades = [ Card.Jack, Card.Five ]
          , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Five, Card.Four, Card.Three ]
          , diamonds = [ Card.Seven, Card.Three ]
          , clubs = [ Card.Six, Card.Two ]
          }

        , { name = "4 HCP, 7 hearts, 5 tricks, equal vulnerability"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Jack, Card.Five ]
          , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Five, Card.Four, Card.Three ]
          , diamonds = [ Card.Seven, Card.Three ]
          , clubs = [ Card.Six, Card.Two ]
          }

        , { name = "4 HCP, 7 hearts, 5 tricks, favorable vulnerability"
          , expected = [Auction.Bid 3 (Just Card.Hearts)]
          , favorability = Vulnerability.Favorable
          , history = []
          , spades = [ Card.Jack, Card.Five ]
          , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Five, Card.Four, Card.Three ]
          , diamonds = [ Card.Seven, Card.Three ]
          , clubs = [ Card.Six, Card.Two ]
          }

        , { name = "4 HCP, 7 hearts, 5 tricks, favorable vulnerability, fourth seat"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Favorable
          , history = [Auction.Pass, Auction.Pass, Auction.Pass]
          , spades = [ Card.Jack, Card.Five ]
          , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Five, Card.Four, Card.Three ]
          , diamonds = [ Card.Seven, Card.Three ]
          , clubs = [ Card.Six, Card.Two ]
          }

        , { name = "5 HCP, 6 spades"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Four, Card.Three, Card.Two ]
          , diamonds = [ Card.Four, Card.Three, Card.Two ]
          , clubs = [ Card.Two ]
          }

        , { name = "5 HCP, 6 hearts"
          , expected = [Auction.Bid 2 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Four, Card.Three, Card.Two ]
          , hearts = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Four, Card.Three, Card.Two ]
          , clubs = [ Card.Two ]
          }

        , { name = "5 HCP, 6 diamonds"
          , expected = [Auction.Bid 2 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Four, Card.Three, Card.Two ]
          , hearts = [ Card.Four, Card.Three, Card.Two ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , clubs = [ Card.Two ]
          }

        , { name = "7 HCP, 7 diamonds, 6 playing tricks, unfavorable vulnerability"
          , expected = [Auction.Bid 2 (Just Card.Diamonds)]
          , favorability = Vulnerability.Unfavorable
          , history = []
          , spades = [ Card.Ace, Card.Four ]
          , hearts = [ Card.Nine, Card.Five, Card.Two ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven, Card.Three, Card.Two ]
          , clubs = [ Card.Seven ]
          }

        , { name = "7 HCP, 7 diamonds, 6 playing tricks, equal vulnerability"
          , expected = [Auction.Bid 3 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Four ]
          , hearts = [ Card.Nine, Card.Five, Card.Two ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven, Card.Three, Card.Two ]
          , clubs = [ Card.Seven ]
          }

        , { name = "7 HCP, 7 diamonds, 6 playing tricks, favorable vulnerability"
          , expected = [Auction.Bid 4 (Just Card.Diamonds)]
          , favorability = Vulnerability.Favorable
          , history = []
          , spades = [ Card.Ace, Card.Four ]
          , hearts = [ Card.Nine, Card.Five, Card.Two ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven, Card.Three, Card.Two ]
          , clubs = [ Card.Seven ]
          }

        , { name = "10 HCP, 6 spades"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Four, Card.Three, Card.Two ]
          , diamonds = [ Card.Four, Card.Three, Card.Two ]
          , clubs = [ Card.Queen ]
          }

        , { name = "10 HCP, 6 hearts"
          , expected = [Auction.Bid 2 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Four, Card.Three, Card.Two ]
          , hearts = [ Card.Ace, Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Four, Card.Three, Card.Two ]
          , clubs = [ Card.Queen ]
          }

        , { name = "10 HCP, 6 diamonds"
          , expected = [Auction.Bid 2 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Four, Card.Three, Card.Two ]
          , hearts = [ Card.Four, Card.Three, Card.Two ]
          , diamonds = [ Card.Ace, Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Queen ]
          }

        , { name = "10 HCP, 8 clubs, 8 playing tricks, unfavorable vulnerability"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Unfavorable
          , history = []
          , spades = []
          , hearts = [ Card.Five, Card.Four, Card.Two ]
          , diamonds = [ Card.Jack, Card.Eight ]
          , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Seven, Card.Six, Card.Three ]
          }

        , { name = "10 HCP, 8 clubs, 8 playing tricks, equal vulnerability"
          , expected = [Auction.Bid 5 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = []
          , hearts = [ Card.Five, Card.Four, Card.Two ]
          , diamonds = [ Card.Jack, Card.Eight ]
          , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Seven, Card.Six, Card.Three ]
          }

        , { name = "10 HCP, 8 clubs, 8 playing tricks, favorable vulnerability"
          , expected = [Auction.Bid 5 (Just Card.Clubs)]
          , favorability = Vulnerability.Favorable
          , history = []
          , spades = []
          , hearts = [ Card.Five, Card.Four, Card.Two ]
          , diamonds = [ Card.Jack, Card.Eight ]
          , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Seven, Card.Six, Card.Three ]
          }

        , { name = "11 HCP, 7 hearts, first seat"
          , expected = [Auction.Bid 1 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Seven, Card.Four ]
          , hearts = [ Card.Ace, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven, Card.Three ]
          , diamonds = [ Card.Ace, Card.Eight, Card.Two ]
          , clubs = [ Card.Four ]
          }

        , { name = "11 HCP, 7 hearts, second seat"
          , expected = [Auction.Bid 1 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass ]
          , spades = [ Card.Seven, Card.Four ]
          , hearts = [ Card.Ace, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven, Card.Three ]
          , diamonds = [ Card.Ace, Card.Eight, Card.Two ]
          , clubs = [ Card.Four ]
          }

        , { name = "11 HCP, 7 hearts, third seat"
          , expected = [Auction.Bid 1 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Pass ]
          , spades = [ Card.Seven, Card.Four ]
          , hearts = [ Card.Ace, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven, Card.Three ]
          , diamonds = [ Card.Ace, Card.Eight, Card.Two ]
          , clubs = [ Card.Four ]
          }

        , { name = "11 HCP, 7 hearts, fourth seat"
          , expected = [Auction.Bid 3 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Pass, Auction.Pass ]
          , spades = [ Card.Seven, Card.Four ]
          , hearts = [ Card.Ace, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven, Card.Three ]
          , diamonds = [ Card.Ace, Card.Eight, Card.Two ]
          , clubs = [ Card.Four ]
          }

        , { name = "13 points, 5 spades"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Queen, Card.Ten ]
          , clubs = [ Card.Jack, Card.Ten ]
          }

        , { name = "13 points, 5 hearts"
          , expected = [Auction.Bid 1 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Queen, Card.Ten ]
          , clubs = [ Card.Jack, Card.Ten ]
          }

        , { name = "13 points, 3 diamonds"
          , expected = [Auction.Bid 1 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Nine ]
          , clubs = [ Card.Queen, Card.Ten ]
          }

        , { name = "13 points, 3 clubs"
          , expected = [Auction.Bid 1 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine ]
          , diamonds = [ Card.Queen, Card.Ten ]
          , clubs = [ Card.Queen, Card.Ten, Card.Nine ]
          }

        , { name = "13 points, 6 spades, 6 hearts"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten ]
          , clubs = []
          }

        , { name = "13 points, 6 spades, 5 hearts"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten ]
          , clubs = [ Card.Eight ]
          }

        , { name = "13 points, 5 spades, 6 hearts"
          , expected = [Auction.Bid 1 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven ]
          , diamonds = [ Card.Ten ]
          , clubs = [ Card.Eight ]
          }

        , { name = "13 points, 5 spades, 5 hearts"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Seven ]
          , clubs = [ Card.Eight ]
          }

        , { name = "13 points, 4 diamonds, 4 clubs"
          , expected = [Auction.Bid 1 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Jack ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Eight, Card.Seven ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
          }

        , { name = "13 points, 4 diamonds, 3 clubs"
          , expected = [Auction.Bid 1 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Jack, Card.Eight ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Eight, Card.Seven ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
          }

        , { name = "13 points, 3 diamonds, 4 clubs"
          , expected = [Auction.Bid 1 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Jack, Card.Eight ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Eight ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Seven ]
          }

        , { name = "13 points, 3 diamonds, 3 clubs"
          , expected = [Auction.Bid 1 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Jack, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Eight ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
          }

        , { name = "13 points, 6 spades, 6 clubs"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Two ]
          , diamonds = []
          , clubs = [ Card.Ace, Card.Jack, Card.Six, Card.Five, Card.Four, Card.Three ]
          }

        , { name = "13 points, 6 spades, 5 clubs"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Two ]
          , diamonds = [ Card.Three ]
          , clubs = [ Card.Ace, Card.Queen, Card.Six, Card.Five, Card.Four ]
          }

        , { name = "13 points, 5 spades, 6 clubs"
          , expected = [Auction.Bid 1 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Six, Card.Five, Card.Four ]
          , hearts = [ Card.Two ]
          , diamonds = [ Card.Three ]
          , clubs = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          }

        , { name = "13 points, 5 spades, 5 clubs"
          , expected = [Auction.Bid 1 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Seven, Card.Two ]
          , diamonds = [ Card.Three ]
          , clubs = [ Card.Ace, Card.Jack, Card.Six, Card.Five, Card.Four ]
          }

        , { name = "15 HCP, 5-3-3-2 distribution, 5 card major"
          , expected = [Auction.Bid 1 (Just Card.Spades), Auction.Bid 1 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight, Card.Two ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight ]
          }

        , { name = "15 HCP, 4-3-3-3 distribution"
          , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 1 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight, Card.Six ]
          }

        , { name = "17 HCP, 9 playing tricks with spades"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ace, Card.King, Card.Ten ]
          , diamonds = [ Card.Six, Card.Five, Card.Three ]
          , clubs = []
          }

        , { name = "17 HCP, 9 playing tricks with herats"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Ten ]
          , hearts = [ Card.Ace, Card.King, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Six, Card.Five, Card.Three ]
          , clubs = []
          }

        , { name = "17 HCP, 4-3-3-3 distribution"
          , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 1 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight, Card.Six ]
          }

        , { name = "20 HCP, 9 playing tricks with spades"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ace, Card.King, Card.Six ]
          , diamonds = [ Card.Ace, Card.Three, Card.Two ]
          , clubs = [ Card.Five ]
          }

        , { name = "20 HCP, 10 playing tricks with clubs"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Five ]
          , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten ]
          , diamonds = [ Card.Ace, Card.King ]
          , clubs = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          }

        , { name = "20 HCP, 4-3-3-3 distribution"
          , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight, Card.Six ]
          }

        , { name = "21 HCP, 4-3-3-3 distribution"
          , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Queen, Card.Eight, Card.Six ]
          }

        , { name = "23 HCP"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen ]
          , hearts = [ Card.Ace, Card.King, Card.Six ]
          , diamonds = [ Card.Ace, Card.Three, Card.Two ]
          , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Four ]
        }

        , { name = "25 HCP, 4-3-3-3 distribution"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Ace, Card.Queen, Card.Six ]
          }

        , { name = "27 HCP, 4-3-3-3 distribution"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.King, Card.Four ]
          , clubs = [ Card.Ace, Card.Queen, Card.Six ]
          }
        ]
  in
    ElmTest.suite "opening" (existenceTests ++ unitTests)


oneNoTrumpResponseSuite : ElmTest.Test
oneNoTrumpResponseSuite =
  let
    existenceTest =
      Bidding.TestUtils.suggestionsExist Bidding.StandardAmerican.system [ Auction.Pass, Auction.Bid 1 Nothing ]
    unitTests =
      List.map (Bidding.TestUtils.testBid Bidding.StandardAmerican.system)
        [ { name = "0 HCP, 4 spades, 4 hearts"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "0 HCP, 5 diamonds, 5 clubs"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          }

        , { name = "0 HCP, 5 spades"
          , expected = [Auction.Bid 2 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "0 HCP, 5 hearts"
          , expected = [Auction.Bid 2 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "0 HCP, 6 diamonds"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six, Card.Five ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "0 HCP, 6 clubs"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six, Card.Five ]
          }

        , { name = "6 HCP, 6 diamonds"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Nine, Card.Eight ]
          , hearts = [ Card.King, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six, Card.Five ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "6 HCP, 6 clubs"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Nine, Card.Eight ]
          , hearts = [ Card.King, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six, Card.Five ]
          }

        , { name = "7 HCP, 6 clubs"
          , expected = [Auction.Bid 3 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          }

        , { name = "7 HCP, 6 diamonds"
          , expected = [Auction.Bid 3 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "7 HCP, no 5-card major, no 6-card minor"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing]
          , spades = [ Card.King, Card.Nine, Card.Two ]
          , hearts = [ Card.Ten, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Eight, Card.Seven, Card.Five, Card.Three ]
          , clubs = [ Card.Ace, Card.Six, Card.Five ]
          }

        , { name = "7 points, 4 spades, 4 hearts"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.King, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "7 points, 5 diamonds, 5 clubs"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ten, Card.Nine ]
          , hearts = [ Card.King ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          }

        , { name = "8 HCP, 6 clubs"
          , expected = [Auction.Bid 3 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          }

        , { name = "8 HCP, 6 diamonds"
          , expected = [Auction.Bid 3 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "8 HCP, 5 spades"
          , expected = [Auction.Bid 2 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , hearts = [ Card.Ace, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "8 HCP, 5 hearts"
          , expected = [Auction.Bid 2 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "8 points, 4 spades"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Ten, Card.Seven, Card.Two ]
          , hearts = [ Card.Jack, Card.Nine, Card.Eight ]
          , diamonds = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three ]
          }

        , { name = "8 points, 4 hearts"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Jack, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven, Card.Two ]
          , diamonds = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three ]
          }

        , { name = "8 points, 4 spades, 4 hearts"
          , expected = [Auction.Bid 2 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven, Card.Two ]
          , diamonds = [ Card.Jack, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three ]
          }

        , { name = "8 points, 4 spades, 4-3-3-3 distribution"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Jack, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three, Card.Two ]
          }

        , { name = "8 HCP, 2 spades, 3 hearts, 4-4-3-2 distribution"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Four, Card.Three ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Jack, Card.Nine, Card.Eight, Card.Two ]
          , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          }

        , { name = "8 HCP, 3 spades, 3 hearts, 5-3-3-2 distribution"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Eight, Card.Four, Card.Three ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Two ]
          , clubs = [ Card.King, Card.Nine ]
          }

        , { name = "8 HCP, 5-4-2-2 distribution, no 4-card major"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Four, Card.Three ]
          , hearts = [ Card.King, Card.Queen ]
          , diamonds = [ Card.Seven, Card.Six, Card.Four, Card.Three ]
          , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Four, Card.Two ]
          }

          -- Not quite sure about this one.  According to http://www.bridgebum.com/1nt_15_17.php
          -- 3-clubs is for 7-8 HCP and 3NT is for 10-13 HCP.  Let's say 9 HCP and 6 clubs is
          -- better treated as the former.
        , { name = "9 HCP, 6 clubs"
          , expected = [Auction.Bid 3 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Jack, Card.Seven ]
          , hearts = [ Card.Ten ]
          , diamonds = [ Card.Jack, Card.Ten, Card.Six, Card.Four ]
          , clubs = [ Card.Ace, Card.King, Card.Ten, Card.Seven, Card.Five, Card.Four ]
          }

        , { name = "9 points, 4 spades, 4-3-3-3 distribution"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three, Card.Two ]
          }

        , { name = "9 HCP, 2 spades, 3 hearts, 4-4-3-2 distribution"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Four, Card.Three ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Nine, Card.Eight, Card.Two ]
          , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          }

        , { name = "9 HCP, 3 spades, 3 hearts, 5-3-3-2 distribution"
          , expected = [Auction.Bid 2 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Eight, Card.Four, Card.Three ]
          , hearts = [ Card.Ace, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Nine, Card.Eight, Card.Two ]
          , clubs = [ Card.King, Card.Nine ]
          }

        , { name = "10 HCP"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Ten, Card.Nine ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Queen, Card.Ten, Card.Nine ]
          }

        , { name = "13 HCP"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.King, Card.Ten, Card.Nine ]
          }

          -- Not quite sure about this one.  According to http://www.bridgebum.com/1nt_15_17.php
          -- 3NT is for 10-13 HCP, but hands like this are slightly stronger but not good enough
          -- for a slam invite.
        , { name = "14 HCP"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Jack ]
          , hearts = [ Card.King, Card.Three, Card.Two ]
          , diamonds = [ Card.King, Card.Seven, Card.Four ]
          , clubs = [ Card.Ace, Card.Six, Card.Five, Card.Four, Card.Three ]
          }

        , { name = "16 HCP, 2-2-4-5 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Five ]
          , hearts = [ Card.Ace, Card.King ]
          , diamonds = [ Card.Queen, Card.Seven, Card.Six, Card.Four ]
          , clubs = [ Card.Ace, Card.Eight, Card.Five, Card.Three, Card.Two ]
          }

        , { name = "16 points, 6 hearts"
          , expected = [Auction.Bid 3 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "16 points, 6 spades"
          , expected = [Auction.Bid 3 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.King, Card.Ten, Card.Nine ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "16 HCP, 4-4-3-2 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.King, Card.Ten, Card.Nine ]
          , clubs = [ Card.Ace, Card.Ten ]
          }

        , { name = "17 HCP, 5-3-3-2 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Queen, Card.Four, Card.Two ]
          , hearts = [ Card.King, Card.Queen, Card.Jack ]
          , diamonds = [ Card.Four, Card.Three ]
          , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Eight, Card.Four ]
          }

          -- FIXME: These are probably better for general tests for using Gerber

        , { name = "18 points, 6 spades, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.King, Card.Jack, Card.Ten ]
          , diamonds = [ Card.Ace, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "18 points, 6 hearts, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Jack, Card.Ten ]
          , hearts = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "18 points, 6 diamonds, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Jack, Card.Ten ]
          , hearts = [ Card.Ace, Card.Ten ]
          , diamonds = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "18 points, 6 clubs, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Jack, Card.Ten ]
          , hearts = [ Card.Ace, Card.Ten ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          }

        , { name = "18 points, 6 spades, void in diamonds"
          , expected = [Auction.Bid 3 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ace, Card.King, Card.Jack, Card.Ten ]
          , diamonds = []
          , clubs = [ Card.King, Card.Ten, Card.Nine ]
          }

        , { name = "18 points, 6 spades, 2 quick losers in diamonds"
          , expected = [Auction.Bid 3 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ace, Card.Jack ]
          , diamonds = [ Card.Queen, Card.Ten, Card.Nine ]
          , clubs = [ Card.Ace, Card.Ten ]
          }

        -- Not sure what to do with hands like this (where Gerber isn't appropriate
        -- due to the void, but the hand is so strong *something* has to be done).
        -- Really, these situations should be handled by a "slam interest" set of
        -- bidding logic and tests.

        , { name = "23 points, 6 clubs, void"
          , expected = [Auction.Bid 6 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Jack, Card.Three ]
          , hearts = []
          , diamonds = [ Card.Ace, Card.King, Card.Eight, Card.Five ]
          , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Seven, Card.Five ]
          }

        , { name = "23 points, 6 diamonds, void"
          , expected = [Auction.Bid 6 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.King, Card.Jack, Card.Three ]
          , hearts = []
          , diamonds = [ Card.Ace, Card.King, Card.Queen, Card.Ten, Card.Seven, Card.Five ]
          , clubs = [ Card.Ace, Card.King, Card.Eight, Card.Five ]
          }
        ]
  in
    ElmTest.suite "response to 1NT" (existenceTest :: unitTests)


twoNoTrumpResponseSuite : ElmTest.Test
twoNoTrumpResponseSuite =
  let
    history = [ Auction.Pass, Auction.Bid 2 Nothing ]
    existenceTest =
      Bidding.TestUtils.suggestionsExist Bidding.StandardAmerican.system history
    unitTests =
      List.map (Bidding.TestUtils.testBid Bidding.StandardAmerican.system)
        [ { name = "0 HCP, 4 spades, 4 hearts"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "0 HCP, 5 diamonds, 5 clubs"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Ten ]
          , hearts = [ Card.Ten, Card.Nine ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          }

        , { name = "0 HCP, 5 spades"
          , expected = [Auction.Bid 3 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "0 HCP, 5 hearts"
          , expected = [Auction.Bid 3 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "5 points, 4 spades"
          , expected = [Auction.Bid 3 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Jack, Card.Ten, Card.Seven, Card.Two ]
          , hearts = [ Card.Jack, Card.Nine, Card.Eight ]
          , diamonds = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three ]
          }

        , { name = "5 points, 4 hearts"
          , expected = [Auction.Bid 3 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Jack, Card.Nine, Card.Eight ]
          , hearts = [ Card.Jack, Card.Ten, Card.Seven, Card.Two ]
          , diamonds = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three ]
          }

        , { name = "5 points, 4 spades, 4 hearts"
          , expected = [Auction.Bid 3 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Jack, Card.Ten, Card.Seven, Card.Two ]
          , diamonds = [ Card.Jack, Card.Nine, Card.Eight ]
          , clubs = [ Card.Four, Card.Three ]
          }

        , { name = "5 HCP, 2 spades, 3 hearts, 4-4-3-2 distribution"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Four, Card.Three ]
          , hearts = [ Card.Jack, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Jack, Card.Nine, Card.Eight, Card.Two ]
          , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
          }

        , { name = "5 HCP, 3 spades, 3 hearts, 5-3-3-2 distribution"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Eight, Card.Four, Card.Three ]
          , hearts = [ Card.Jack, Card.Ten, Card.Seven ]
          , diamonds = [ Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Two ]
          , clubs = [ Card.King, Card.Nine ]
          }

        , { name = "5 HCP, 5-4-2-2 distribution, no 4-card major"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Four, Card.Three ]
          , hearts = [ Card.Queen, Card.Ten ]
          , diamonds = [ Card.Seven, Card.Six, Card.Four, Card.Three ]
          , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Four, Card.Two ]
          }

        , { name = "5 HCP, 4-3-3-3 distribution, 4 hearts"
          , expected = [Auction.Bid 3 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Jack, Card.Six, Card.Three ]
          , hearts = [ Card.Queen, Card.Jack, Card.Nine, Card.Four ]
          , diamonds = [ Card.Jack, Card.Ten, Card.Six ]
          , clubs = [ Card.Ten, Card.Nine, Card.Six ]
          }

        , { name = "13 HCP, 2-2-4-5 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.King, Card.Five ]
          , hearts = [ Card.Ace, Card.Ten ]
          , diamonds = [ Card.Queen, Card.Seven, Card.Six, Card.Four ]
          , clubs = [ Card.Ace, Card.Eight, Card.Five, Card.Three, Card.Two ]
          }

        , { name = "13 HCP, 4-4-3-2 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ace, Card.Ten ]
          }

        , { name = "13 HCP, 5-3-3-2 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Queen, Card.Four, Card.Two ]
          , hearts = [ Card.King, Card.Queen, Card.Jack ]
          , diamonds = [ Card.Four, Card.Three ]
          , clubs = [ Card.King, Card.Queen, Card.Eight, Card.Four, Card.Three ]
          }

          -- FIXME: These are probably better for general tests for using Gerber

        , { name = "13 points, 6 spades, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , hearts = [ Card.King, Card.Ten ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.King, Card.Ten, Card.Nine ]
          }

        , { name = "13 points, 6 hearts, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "13 points, 6 diamonds, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Ten ]
          , diamonds = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "13 points, 6 clubs, no voids, no 2 quick losers in any suit"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 Nothing ]
          , spades = [ Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.King, Card.Ten ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          }
        ]
  in
    ElmTest.suite "response to 2NT" (existenceTest :: unitTests)


{- FIXME: suggestionsExist crashes when trying to generate hands that
satisfy a 3NT opening.  Need to write a smarter producer that doesn't
have to brute-force the 52! / (13!)^4 search space.
-}
threeNoTrumpResponseSuite : ElmTest.Test
threeNoTrumpResponseSuite =
  let
    history = [ Auction.Pass, Auction.Bid 3 Nothing ]
    unitTests =
      List.map (Bidding.TestUtils.testBid Bidding.StandardAmerican.system)
        [ { name = "5 spades, 3 hearts"
          , expected = [Auction.Bid 4 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "3 spades, 5 hearts"
          , expected = [Auction.Bid 4 (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "4 spades, 4 hearts"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "5 spades, 4 hearts"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "4 spades, 5 hearts"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six ]
          , diamonds = [ Card.Ten, Card.Nine ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "4 spades, 3 hearts, not 4-3-3-3 distribution"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "3 spades, 4 hearts, not 4-3-3-3 distribution"
          , expected = [Auction.Bid 4 (Just Card.Clubs)]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , clubs = [ Card.Ten, Card.Nine ]
          }

        , { name = "4 spades, 3 hearts, 4-3-3-3 distribution"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight ]
          }

        , { name = "3 spades, 4 hearts, 4-3-3-3 distribution"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight ]
          }

        , { name = "3 spades, 3 hearts"
          , expected = [Auction.Pass]
          , favorability = Vulnerability.Equal
          , history = history
          , spades = [ Card.Ten, Card.Nine, Card.Eight ]
          , hearts = [ Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ten, Card.Nine, Card.Eight ]
          , clubs = [ Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          }
        ]
  in
    ElmTest.suite "response to 3NT" unitTests


{- These only check that a convention response is made.  Full testing
that the convention responses are correct is done in the modules that
implement those conventions.
-}
conventionResponseSuite : ElmTest.Test
conventionResponseSuite =
  let
    unitTests =
      List.map (Bidding.TestUtils.testBid Bidding.StandardAmerican.system)
        [ { name = "Stayman"
          , expected = [Auction.Bid 2 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 2 (Just Card.Clubs), Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.King, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Ten, Card.Nine ]
          , diamonds = [ Card.King, Card.Ten, Card.Nine ]
          , clubs = [ Card.Jack, Card.Ten, Card.Nine ]
          }
        ]
  in
    ElmTest.suite "response to conventions" unitTests
