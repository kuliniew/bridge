module Bidding.StandardAmericanTests (all) where

import Auction
import Bidding
import Bidding.StandardAmerican
import Card exposing (Card)

import ElmTest
import Random


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.StandardAmericanTests"
    [ openingSuite
    ]


openingSuite : ElmTest.Test
openingSuite =
  ElmTest.suite "opening"
    [ ElmTest.test "must bid 1NT (1NT example 1)" <|
        let
          hand =
            { spades = [ Card.Ace, Card.Eight, Card.Seven, Card.Three ]
            , hearts = [ Card.Ace, Card.Jack, Card.Four, Card.Two ]
            , diamonds = [ Card.Ace, Card.Jack, Card.Two ]
            , clubs = [ Card.King, Card.Ten ]
          }
        in
          mustBid (Auction.Bid 1 Nothing) [] hand

    , ElmTest.test "may bid 1NT (1NT example 2)" <|
        let
          hand =
            { spades = [ Card.King, Card.Nine, Card.Seven ]
            , hearts = [ Card.Ten, Card.Two ]
            , diamonds = [ Card.King, Card.Eight, Card.Seven ]
            , clubs = [ Card.Ace, Card.King, Card.Queen, Card.Four, Card.Three ]
          }
        in
          mayBid (Auction.Bid 1 Nothing) [] hand

    , ElmTest.test "must bid 1NT (1NT example 3)" <|
        let
          hand =
            { spades = [ Card.Ace, Card.Jack ]
            , hearts = [ Card.Eight, Card.Seven, Card.Six ]
            , diamonds = [ Card.Ace, Card.King, Card.Nine, Card.Eight ]
            , clubs = [ Card.King, Card.Ten, Card.Three, Card.Two ]
          }
        in
          mustBid (Auction.Bid 1 Nothing) [] hand

    , ElmTest.test "must bid 1NT (1NT example 4)" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen ]
            , hearts = [ Card.King, Card.Queen, Card.Jack, Card.Two ]
            , diamonds = [ Card.Jack, Card.Six, Card.Five ]
            , clubs = [ Card.Nine, Card.Seven, Card.Three ]
          }
        in
          mustBid (Auction.Bid 1 Nothing) [] hand
    ]


{-| Type of List.any or List.all, as applied to a bid.
-}
type alias Combiner = (Auction.Bid -> Bool) -> List Auction.Bid -> Bool


{-| Test that a particular bid must be made.
-}
mustBid : Auction.Bid -> List Auction.Bid -> Card.SampleHand -> ElmTest.Assertion
mustBid = checkChoice List.all


{-| Test that a particular bid may be made, but that alternatives are allowed.
-}
mayBid : Auction.Bid -> List Auction.Bid -> Card.SampleHand -> ElmTest.Assertion
mayBid = checkChoice List.any


{-| Run a bidding scenario and check for the expected result.
-}
checkChoice : Combiner -> Auction.Bid -> List Auction.Bid -> Card.SampleHand -> ElmTest.Assertion
checkChoice combiner expected history hand =
  let
    seeds = List.map Random.initialSeed [0 .. 99]
    results = List.map (.bid << fst << Bidding.choose Bidding.StandardAmerican.system (annotate history) (Card.fromSuits hand)) seeds
  in
    ElmTest.assert (combiner (\bid -> bid == expected) results)


{-| Annotate an entire bidding history.
-}
annotate : List Auction.Bid -> List Bidding.AnnotatedBid
annotate =
  let
    augment bid annotatedHistory =
      Bidding.annotate Bidding.StandardAmerican.system annotatedHistory bid :: annotatedHistory
  in
    List.foldr augment []
