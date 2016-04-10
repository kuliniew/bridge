module Bidding.StandardAmericanTests (all) where

import Auction
import Bidding
import Bidding.StandardAmerican
import Card exposing (Card)

import ElmTest
import List.Extra
import Random


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.StandardAmericanTests"
    [ openingSuite
    ]


type alias BidTest =
  { name : String
  , expected : List Auction.Bid
  , history : List Auction.Bid
  , spades : List Card.Rank
  , hearts : List Card.Rank
  , diamonds : List Card.Rank
  , clubs : List Card.Rank
  }


openingSuite : ElmTest.Test
openingSuite =
  ElmTest.suite "opening" <| List.map testBid
    [ { name = "5 HCP, 6 spades"
      , expected = [Auction.Bid 2 (Just Card.Spades)]
      , history = []
      , spades = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.Four, Card.Three, Card.Two ]
      , diamonds = [ Card.Four, Card.Three, Card.Two ]
      , clubs = [ Card.Two ]
      }

    , { name = "5 HCP, 6 hearts"
      , expected = [Auction.Bid 2 (Just Card.Hearts)]
      , history = []
      , spades = [ Card.Four, Card.Three, Card.Two ]
      , hearts = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , diamonds = [ Card.Four, Card.Three, Card.Two ]
      , clubs = [ Card.Two ]
      }

    , { name = "5 HCP, 6 diamonds"
      , expected = [Auction.Bid 2 (Just Card.Diamonds)]
      , history = []
      , spades = [ Card.Four, Card.Three, Card.Two ]
      , hearts = [ Card.Four, Card.Three, Card.Two ]
      , diamonds = [ Card.Ace, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , clubs = [ Card.Two ]
      }

    , { name = "10 HCP, 6 spades"
      , expected = [Auction.Bid 2 (Just Card.Spades)]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.Four, Card.Three, Card.Two ]
      , diamonds = [ Card.Four, Card.Three, Card.Two ]
      , clubs = [ Card.Queen ]
      }

    , { name = "10 HCP, 6 hearts"
      , expected = [Auction.Bid 2 (Just Card.Hearts)]
      , history = []
      , spades = [ Card.Four, Card.Three, Card.Two ]
      , hearts = [ Card.Ace, Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
      , diamonds = [ Card.Four, Card.Three, Card.Two ]
      , clubs = [ Card.Queen ]
      }

    , { name = "10 HCP, 6 diamonds"
      , expected = [Auction.Bid 2 (Just Card.Diamonds)]
      , history = []
      , spades = [ Card.Four, Card.Three, Card.Two ]
      , hearts = [ Card.Four, Card.Three, Card.Two ]
      , diamonds = [ Card.Ace, Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
      , clubs = [ Card.Queen ]
      }

    , { name = "13 points, 5 spades"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
      , diamonds = [ Card.Queen, Card.Ten ]
      , clubs = [ Card.Jack, Card.Ten ]
      }

    , { name = "13 points, 5 hearts"
      , expected = [Auction.Bid 1 (Just Card.Hearts)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
      , diamonds = [ Card.Queen, Card.Ten ]
      , clubs = [ Card.Jack, Card.Ten ]
      }

    , { name = "13 points, 3 diamonds"
      , expected = [Auction.Bid 1 (Just Card.Diamonds)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Nine ]
      , clubs = [ Card.Queen, Card.Ten ]
      }

    , { name = "13 points, 3 clubs"
      , expected = [Auction.Bid 1 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine ]
      , diamonds = [ Card.Queen, Card.Ten ]
      , clubs = [ Card.Queen, Card.Ten, Card.Nine ]
      }

    , { name = "13 points, 6 spades, 6 hearts"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
      , diamonds = [ Card.Ten ]
      , clubs = []
      }

    , { name = "13 points, 6 spades, 5 hearts"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine ]
      , diamonds = [ Card.Ten ]
      , clubs = [ Card.Eight ]
      }

    , { name = "13 points, 5 spades, 6 hearts"
      , expected = [Auction.Bid 1 (Just Card.Hearts)]
      , history = []
      , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Seven ]
      , diamonds = [ Card.Ten ]
      , clubs = [ Card.Eight ]
      }

    , { name = "13 points, 5 spades, 5 hearts"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.King, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine ]
      , diamonds = [ Card.Ten, Card.Seven ]
      , clubs = [ Card.Eight ]
      }

    , { name = "13 points, 4 diamonds, 4 clubs"
      , expected = [Auction.Bid 1 (Just Card.Diamonds)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine ]
      , hearts = [ Card.King, Card.Jack ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Eight, Card.Seven ]
      , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
      }

    , { name = "13 points, 4 diamonds, 3 clubs"
      , expected = [Auction.Bid 1 (Just Card.Diamonds)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine ]
      , hearts = [ Card.King, Card.Jack, Card.Eight ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Eight, Card.Seven ]
      , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
      }

    , { name = "13 points, 3 diamonds, 4 clubs"
      , expected = [Auction.Bid 1 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine ]
      , hearts = [ Card.King, Card.Jack, Card.Eight ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Eight ]
      , clubs = [ Card.Ace, Card.Ten, Card.Nine, Card.Seven ]
      }

    , { name = "13 points, 3 diamonds, 3 clubs"
      , expected = [Auction.Bid 1 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.King, Card.Ten, Card.Nine ]
      , hearts = [ Card.King, Card.Jack, Card.Eight, Card.Seven ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Eight ]
      , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
      }

    , { name = "13 points, 6 spades, 6 clubs"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.Two ]
      , diamonds = []
      , clubs = [ Card.Ace, Card.Jack, Card.Six, Card.Five, Card.Four, Card.Three ]
      }

    , { name = "13 points, 6 spades, 5 clubs"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.Two ]
      , diamonds = [ Card.Three ]
      , clubs = [ Card.Ace, Card.Queen, Card.Six, Card.Five, Card.Four ]
      }

    , { name = "13 points, 5 spades, 6 clubs"
      , expected = [Auction.Bid 1 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.Ace, Card.Queen, Card.Six, Card.Five, Card.Four ]
      , hearts = [ Card.Two ]
      , diamonds = [ Card.Three ]
      , clubs = [ Card.King, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      }

    , { name = "13 points, 5 spades, 5 clubs"
      , expected = [Auction.Bid 1 (Just Card.Spades)]
      , history = []
      , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.Seven, Card.Two ]
      , diamonds = [ Card.Three ]
      , clubs = [ Card.Ace, Card.Jack, Card.Six, Card.Five, Card.Four ]
      }

    , { name = "15 HCP, 5-3-3-2 distribution, 5 card major"
      , expected = [Auction.Bid 1 (Just Card.Spades), Auction.Bid 1 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight, Card.Two ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
      , clubs = [ Card.Jack, Card.Eight ]
      }

    , { name = "15 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 1 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
      , clubs = [ Card.Jack, Card.Eight, Card.Six ]
      }

    , { name = "17 HCP, 9 playing tricks with spades"
      , expected = [Auction.Bid 2 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen, Card.Jack, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.Ace, Card.King, Card.Ten ]
      , diamonds = [ Card.Six, Card.Five, Card.Three ]
      , clubs = []
      }

    , { name = "17 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 1 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
      , clubs = [ Card.Jack, Card.Eight, Card.Six ]
      }

    , { name = "20 HCP, 9 playing tricks with spades"
      , expected = [Auction.Bid 2 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen, Card.Nine, Card.Eight, Card.Seven ]
      , hearts = [ Card.Ace, Card.King, Card.Six ]
      , diamonds = [ Card.Ace, Card.Three, Card.Two ]
      , clubs = [ Card.Five ]
      }

    , { name = "20 HCP, 10 playing tricks with clubs"
      , expected = [Auction.Bid 2 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.Five ]
      , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Ten ]
      , diamonds = [ Card.Ace, Card.King ]
      , clubs = [ Card.Ace, Card.King, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      }

    , { name = "20 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 2 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
      , clubs = [ Card.Jack, Card.Eight, Card.Six ]
      }

    , { name = "21 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 1 (Just Card.Clubs), Auction.Bid 2 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
      , clubs = [ Card.Queen, Card.Eight, Card.Six ]
      }

    , { name = "23 HCP"
      , expected = [Auction.Bid 2 (Just Card.Clubs)]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen ]
      , hearts = [ Card.Ace, Card.King, Card.Six ]
      , diamonds = [ Card.Ace, Card.Three, Card.Two ]
      , clubs = [ Card.King, Card.Ten, Card.Nine, Card.Four ]
    }

    , { name = "25 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 3 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
      , clubs = [ Card.Ace, Card.Queen, Card.Six ]
      }

    , { name = "27 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 3 Nothing]
      , history = []
      , spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
      , hearts = [ Card.King, Card.Queen, Card.Seven ]
      , diamonds = [ Card.Ace, Card.King, Card.Four ]
      , clubs = [ Card.Ace, Card.Queen, Card.Six ]
      }
    ]


{-| Test that the expectd bids are suggested in a situation.
-}
testBid : BidTest -> ElmTest.Test
testBid test =
  let
    chosen =
      Bidding.viableChoices Bidding.StandardAmerican.system (annotate test.history) (Card.fromSuits test)
        |> List.map .bid
    message = "expected " ++ toString (test.expected) ++ " but got " ++ toString (chosen)
  in
    ElmTest.test test.name <|
      if chosen `List.Extra.isPermutationOf` test.expected
        then ElmTest.pass
        else ElmTest.fail message


{-| Annotate an entire bidding history.
-}
annotate : List Auction.Bid -> List Bidding.AnnotatedBid
annotate =
  let
    augment bid annotatedHistory =
      Bidding.annotate Bidding.StandardAmerican.system annotatedHistory bid :: annotatedHistory
  in
    List.foldr augment []
