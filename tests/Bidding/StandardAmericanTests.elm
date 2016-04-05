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
    [ ElmTest.test "1NT allowed for 5-3-3-2 distribution and 15 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight, Card.Two ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
            , clubs = [ Card.Jack, Card.Eight ]
            }
        in
          mayBid (Auction.Bid 1 Nothing) [] hand

    , ElmTest.test "1NT required for 4-3-3-3 distribution and 15 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Queen, Card.Jack, Card.Four ]
            , clubs = [ Card.Jack, Card.Eight, Card.Six ]
            }
        in
          mustBid (Auction.Bid 1 Nothing) [] hand

    , ElmTest.test "1NT required for 4-3-3-3 distribution and 17 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.Queen, Card.Ten, Card.Eight ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
            , clubs = [ Card.Jack, Card.Eight, Card.Six ]
            }
        in
          mustBid (Auction.Bid 1 Nothing) [] hand

    , ElmTest.test "2NT required for 4-3-3-3 distribution and 20 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
            , clubs = [ Card.Jack, Card.Eight, Card.Six ]
            }
        in
          mustBid (Auction.Bid 2 Nothing) [] hand

    , ElmTest.test "2NT required for 4-3-3-3 distribution and 21 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
            , clubs = [ Card.Queen, Card.Eight, Card.Six ]
            }
        in
          mustBid (Auction.Bid 2 Nothing) [] hand

    , ElmTest.test "3NT required for 4-3-3-3 distribution and 25 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Ace, Card.Jack, Card.Four ]
            , clubs = [ Card.Ace, Card.Queen, Card.Six ]
            }
        in
          mustBid (Auction.Bid 3 Nothing) [] hand

    , ElmTest.test "3NT required for 4-3-3-3 distribution and 27 HCP" <|
        let
          hand =
            { spades = [ Card.Ace, Card.King, Card.Queen, Card.Eight ]
            , hearts = [ Card.King, Card.Queen, Card.Seven ]
            , diamonds = [ Card.Ace, Card.King, Card.Four ]
            , clubs = [ Card.Ace, Card.Queen, Card.Six ]
            }
        in
          mustBid (Auction.Bid 3 Nothing) [] hand
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
