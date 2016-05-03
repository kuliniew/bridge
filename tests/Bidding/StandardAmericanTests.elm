module Bidding.StandardAmericanTests (all) where

import Auction
import Bidding
import Bidding.StandardAmerican
import Card exposing (Card)
import Card.Producer
import TestUtils
import Vulnerability
import Vulnerability.Producer

import Check
import Check.Producer
import ElmTest
import List.Extra
import Random


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.StandardAmericanTests"
    [ openingSuite
    , oneNoTrumpResponseSuite
    ]


type alias BidTest =
  { name : String
  , expected : List Auction.Bid
  , favorability : Vulnerability.Favorability
  , history : List Auction.Bid
  , spades : List Card.Rank
  , hearts : List Card.Rank
  , diamonds : List Card.Rank
  , clubs : List Card.Rank
  }


openingSuite : ElmTest.Test
openingSuite =
  let
    existenceTests =
      List.map suggestionsExist <| List.Extra.inits (List.repeat 3 Auction.Pass)
    unitTests =
      List.map testBid
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
      suggestionsExist [ Auction.Pass, Auction.Bid 1 Nothing ]
    unitTests =
      List.map testBid
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

        , { name = "17 points, 6 hearts"
          , expected = [Auction.Bid 3 (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "17 points, 6 spades"
          , expected = [Auction.Bid 3 (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
          , hearts = [ Card.Ace, Card.Ten, Card.Nine ]
          , diamonds = [ Card.King, Card.Ten ]
          , clubs = [ Card.King, Card.Ten ]
          }

        , { name = "17 HCP, 4-4-3-2 distribution"
          , expected = [Auction.Bid 4 Nothing]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid 1 Nothing ]
          , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine ]
          , hearts = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
          , diamonds = [ Card.Ace, Card.Ten, Card.Nine ]
          , clubs = [ Card.Ace, Card.Ten ]
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
        ]
  in
    ElmTest.suite "response to 1NT" (existenceTest :: unitTests)


{-| Test that bids are suggested regardless of the hand or vulnerability.
-}
suggestionsExist : List Auction.Bid -> ElmTest.Test
suggestionsExist history =
  let
    suggestionsExist' favorability hand =
      not <| List.isEmpty <| Bidding.viableChoices Bidding.StandardAmerican.system favorability (annotate favorability history) hand
  in
    TestUtils.generativeTest <|
      Check.claim
        ("bids are always suggested for " ++ toString history)
      `Check.true`
        uncurry suggestionsExist'
      `Check.for`
        Check.Producer.tuple (Vulnerability.Producer.favorability, Card.Producer.hand)


{-| Test that the expectd bids are suggested in a situation.
-}
testBid : BidTest -> ElmTest.Test
testBid test =
  let
    chosen =
      Bidding.viableChoices Bidding.StandardAmerican.system test.favorability (annotate test.favorability test.history) (Card.fromSuits test)
        |> List.map .bid
    message = "expected " ++ toString (test.expected) ++ " but got " ++ toString (chosen)
  in
    ElmTest.test test.name <|
      if chosen `List.Extra.isPermutationOf` test.expected
        then ElmTest.pass
        else ElmTest.fail message


{-| Annotate an entire bidding history.
-}
annotate : Vulnerability.Favorability -> List Auction.Bid -> List Bidding.AnnotatedBid
annotate favorability history =
  let
    augment (fav, bid) annotatedHistory =
      Bidding.annotate Bidding.StandardAmerican.system fav annotatedHistory bid :: annotatedHistory
    tagHistory fav hist =
      case hist of
        [] -> []
        bid :: rest -> (fav, bid) :: tagHistory (Vulnerability.opposing fav) rest
    taggedHistory = tagHistory (Vulnerability.opposing favorability) history
  in
    List.foldr augment [] taggedHistory
