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
  , hand : Card.SampleHand
  }


openingSuite : ElmTest.Test
openingSuite =
  ElmTest.suite "opening" <| List.map testBid
    [ { name = "15 HCP, 5-3-3-2 distribution, 5 card major"
      , expected = [Auction.Bid 1 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight, Card.Two ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight ]
          }
      }

    , { name = "15 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 1 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight, Card.Six ]
          }
      }

    , { name = "17 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 1 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight, Card.Six ]
          }
      }

    , { name = "20 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 2 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Jack, Card.Eight, Card.Six ]
          }
      }

    , { name = "21 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 2 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Queen, Card.Eight, Card.Six ]
          }
        }

    , { name = "25 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 3 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
          , clubs = [ Card.Ace, Card.Queen, Card.Six ]
          }
        }

    , { name = "27 HCP, 4-3-3-3 distribution"
      , expected = [Auction.Bid 3 Nothing]
      , history = []
      , hand =
          { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
          , hearts = [ Card.King, Card.Queen, Card.Seven ]
          , diamonds = [ Card.Ace, Card.King, Card.Four ]
          , clubs = [ Card.Ace, Card.Queen, Card.Six ]
          }
      }
    ]


{-| Test that the expectd bids are suggested in a situation.
-}
testBid : BidTest -> ElmTest.Test
testBid test =
  let
    chosen =
      Bidding.viableChoices Bidding.StandardAmerican.system (annotate test.history) (Card.fromSuits test.hand)
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
