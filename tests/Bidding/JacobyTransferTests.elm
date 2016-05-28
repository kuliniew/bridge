module Bidding.JacobyTransferTests exposing (all)

import Auction
import Bidding
import Bidding.JacobyTransfer
import Bidding.TestUtils
import Card
import Vulnerability

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.JacobyTransferTests"
    [ unconditionedBidSuite 2
    , unconditionedBidSuite 3
    , unconditionedBidSuite 4
    , conditionedBidSuite 2
    , conditionedBidSuite 3
    , conditionedBidSuite 4
    ]


unconditionedBidSuite : Int -> ElmTest.Test
unconditionedBidSuite level =
  let
    name =
      "bid - " ++ toString level ++ " level - no extra conditions"
    system =
      { name = "Jacoby Transfer test (no extra conditions)"
      , suggestions = \ _ _ -> Bidding.JacobyTransfer.bids level Nothing
      }
    unitTests =
      List.map (Bidding.TestUtils.testBid system)
        [ { name = "5 spades, 4 hearts"
          , expected = [Auction.Bid level (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ace, Card.Two ]
          , clubs = [ Card.Ace, Card.Two ]
          }

        , { name = "4 spades, 5 hearts"
          , expected = [Auction.Bid level (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , diamonds = [ Card.Ace, Card.Two ]
          , clubs = [ Card.Ace, Card.Two ]
          }

        , { name = "5 spades, 5 hearts"
          , expected = [Auction.Bid level (Just Card.Hearts), Auction.Bid level (Just Card.Diamonds)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , diamonds = [ Card.Ace ]
          , clubs = [ Card.Ace, Card.Two ]
          }

        , { name = "4 spades, 4 hearts"
          , expected = []
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Ace, Card.Two ]
          }
        ]
  in
    ElmTest.suite name unitTests


conditionedBidSuite : Int -> ElmTest.Test
conditionedBidSuite level =
  let
    name =
      "bid - " ++ toString level ++ " level - with extra condition"
    system =
      { name = "Jacoby Transfer test (with extra conditions)"
      , suggestions = \ _ _ -> Bidding.JacobyTransfer.bids level (Just <| Bidding.Minimum Bidding.HighCardPoints (Bidding.Constant 8))
      }
    unitTests =
      List.map (Bidding.TestUtils.testBid system)
        [ { name = "only meets base requirements"
          , expected = []
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ten, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ten, Card.Two ]
          , clubs = [ Card.Ten, Card.Two ]
          }

        , { name = "only meets additional requirements"
          , expected = []
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ten, Card.Two, Card.Three ]
          , clubs = [ Card.Ten, Card.Two ]
          }

        , { name = "meets all requirements"
          , expected = [Auction.Bid level (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ten, Card.Two ]
          , clubs = [ Card.Ten, Card.Two ]
          }
        ]
  in
    ElmTest.suite name unitTests
