module Bidding.JacobyTransferTests exposing (all)

import Auction
import Bidding
import Bidding.ConventionResponse
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
    , responseSuite 2 3
    , responseSuite 3 3
    , responseSuite 4 4
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


responseSuite : Int -> Int -> ElmTest.Test
responseSuite acceptLevel superacceptLevel =
  let
    bidLevel =
      acceptLevel
    name =
      "responses - " ++ toString bidLevel ++ " level"
    system =
      { name = "Jacoby Transfer test (no extra conditions)"
      , suggestions = Bidding.ConventionResponse.withConventionResponse <|
          \ fav hist ->
            case hist of
              [] -> Bidding.JacobyTransfer.bids bidLevel Nothing
              _ -> []
      }
    unitTests =
      List.map (Bidding.TestUtils.testBid system)
        [ { name = "to spades - 0 spades, 0 HCP"
          , expected = [Auction.Bid acceptLevel (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid bidLevel (Just Card.Hearts) ]
          , spades = []
          , hearts = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ten, Card.Two, Card.Three, Card.Four, Card.Five ]
          , clubs = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          }

        , { name = "to hearts - 0 hearts, 0 HCP"
          , expected = [Auction.Bid acceptLevel (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid bidLevel (Just Card.Diamonds) ]
          , spades = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          , hearts = []
          , diamonds = [ Card.Ten, Card.Two, Card.Three, Card.Four, Card.Five ]
          , clubs = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          }

        , { name = "to spades - 4 spades, 0 HCP"
          , expected = [Auction.Bid acceptLevel (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid bidLevel (Just Card.Hearts) ]
          , spades = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ten, Card.Two, Card.Three ]
          , diamonds = [ Card.Ten, Card.Two, Card.Three ]
          , clubs = [ Card.Ten, Card.Two, Card.Three ]
          }

        , { name = "to hearts - 4 hearts, 0 HCP"
          , expected = [Auction.Bid acceptLevel (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid bidLevel (Just Card.Diamonds) ]
          , spades = [ Card.Ten, Card.Two, Card.Three ]
          , hearts = [ Card.Ten, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ten, Card.Two, Card.Three ]
          , clubs = [ Card.Ten, Card.Two, Card.Three ]
          }

        , { name = "to spades - 4 spades, 17 points"
          , expected = [Auction.Bid superacceptLevel (Just Card.Spades)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid bidLevel (Just Card.Hearts) ]
          , spades = [ Card.Ace, Card.King, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three ]
          , diamonds = [ Card.King, Card.Two, Card.Three ]
          , clubs = [ Card.King, Card.Two, Card.Three ]
          }

        , { name = "to hearts - 4 hearts, 17 points"
          , expected = [Auction.Bid superacceptLevel (Just Card.Hearts)]
          , favorability = Vulnerability.Equal
          , history = [ Auction.Pass, Auction.Bid bidLevel (Just Card.Diamonds) ]
          , spades = [ Card.Ace, Card.Two, Card.Three ]
          , hearts = [ Card.Ace, Card.King, Card.Three, Card.Four ]
          , diamonds = [ Card.King, Card.Two, Card.Three ]
          , clubs = [ Card.King, Card.Two, Card.Three ]
          }
        ]
  in
    ElmTest.suite name unitTests
