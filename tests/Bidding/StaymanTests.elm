module Bidding.StaymanTests (all) where

import Auction
import Bidding
import Bidding.ConventionResponse
import Bidding.Stayman
import Bidding.TestUtils
import Card
import Vulnerability

import Array
import ElmTest
import Maybe.Extra
import String


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.StaymanTests"
    [ bidSuite
    , responseSuite 2
    , responseSuite 3
    , responseSuite 4
    ]


bidSuite : ElmTest.Test
bidSuite =
  let
    yes =
      Just <| Auction.Bid 2 (Just Card.Clubs)
    no =
      Nothing
    history =
      []
    simpleSystem =
      { name = "Stayman test (no extra conditions)"
      , suggestions = \ _ _ -> [Bidding.Stayman.bid 2 Nothing]
      }
    simpleUnitTests =
      List.map (Bidding.TestUtils.testBid simpleSystem)
        [ testCase no history 3 3 5 2
        , testCase yes history 4 4 3 2
        , testCase yes history 5 5 2 1

        , testCase yes history 4 3 4 2
        , testCase no history 4 3 3 3

        , testCase yes history 3 4 4 2
        , testCase no history 3 4 3 3

        , testCase no history 5 3 3 2
        , testCase yes history 5 4 2 2

        , testCase no history 3 5 3 2
        , testCase yes history 4 5 2 2
        ]
    conditionedSystem =
      { name = "Stayman test (with extra condition)"
      , suggestions = \ _ _ -> [Bidding.Stayman.bid 2 (Just <| Bidding.Minimum Bidding.HighCardPoints (Bidding.Constant 8))]
      }
    conditionedUnitTests =
      List.map (Bidding.TestUtils.testBid conditionedSystem)
        [ { name = "only meets base requirements"
          , expected = Maybe.Extra.maybeToList no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Two, Card.Three, Card.Four, Card.Five ]
          , diamonds = [ Card.Two, Card.Three, Card.Four ]
          , clubs = [ Card.Two, Card.Three ]
          }

        , { name = "only meets additional requirements"
          , expected = Maybe.Extra.maybeToList no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Three, Card.Four ]
          , diamonds = [ Card.Two, Card.Three, Card.Four, Card.Five ]
          , clubs = [ Card.Two, Card.Three, Card.Four ]
          }

        , { name = "meets all requirements"
          , expected = Maybe.Extra.maybeToList yes
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ace, Card.Three, Card.Four, Card.Five ]
          , diamonds = [ Card.Two, Card.Three, Card.Four ]
          , clubs = [ Card.Two, Card.Three ]
          }
        ]
  in
    ElmTest.suite "bid" (simpleUnitTests ++ conditionedUnitTests)


responseSuite : Int -> ElmTest.Test
responseSuite level =
  let
    name =
      "response - " ++ toString level ++ " level"
    diamonds =
      Just <| Auction.Bid level (Just Card.Diamonds)
    hearts =
      Just <| Auction.Bid level (Just Card.Hearts)
    spades =
      Just <| Auction.Bid level (Just Card.Spades)
    validHistory =
      [Auction.Pass, Auction.Bid level (Just Card.Clubs)]
    invalidHistory =
      [Auction.Pass, Auction.Bid level (Just Card.Diamonds), Auction.Pass, Auction.Bid level (Just Card.Clubs)]
    simpleSystem =
      { name = "Stayman test (no extra conditions)"
      , suggestions = \ fav hist ->
          case Bidding.ConventionResponse.conventionResponse fav hist of
            Just response ->
              response
            Nothing ->
              case hist of
                [] -> [Bidding.Stayman.bid level Nothing]
                _ -> []
      }
    unitTests =
      List.map (Bidding.TestUtils.testBid simpleSystem)
        [ testCase diamonds validHistory 3 3 4 3
        , testCase hearts validHistory 4 4 3 2
        , testCase hearts validHistory 5 4 2 2
        , testCase hearts validHistory 4 5 2 2
        , testCase spades validHistory 4 3 3 3
        , testCase spades validHistory 5 3 3 2

        , testCase Nothing invalidHistory 4 4 3 2
        ]
  in
    ElmTest.suite name unitTests


testCase : Maybe Auction.Bid -> List Auction.Bid -> Int -> Int -> Int -> Int -> Bidding.TestUtils.BidTest
testCase expected history spades hearts diamonds clubs =
  let
    distribution =
      List.sortBy negate [spades, hearts, diamonds, clubs]
        |> List.map toString
        |> String.join "-"
  in
    { name = toString spades ++ " spades, " ++ toString hearts ++ " hearts, " ++ distribution ++ " distribution"
    , expected = Maybe.Extra.maybeToList expected
    , favorability = Vulnerability.Equal
    , history = history
    , spades = cards spades
    , hearts = cards hearts
    , diamonds = cards diamonds
    , clubs = cards clubs
    }


cards : Int -> List Card.Rank
cards n = List.take n (Array.toList Card.ranks)
