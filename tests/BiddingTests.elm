module BiddingTests (all) where

import Auction
import Bidding

import ElmTest
import Random


all : ElmTest.Test
all =
  ElmTest.suite "Bidding"
    [ annotateSuite
    , chooseSuite
    ]


annotateSuite : ElmTest.Test
annotateSuite =
  ElmTest.suite "annotate"
    [ ElmTest.test "takes annotations from the system" <|
        ElmTest.assertEqual twoNoTrump (Bidding.annotate testSystem [] (Auction.Bid 2 Nothing))

    , ElmTest.test "falls back to OutOfSystem" <|
        let
          expected =
            { bid = Auction.Bid 3 Nothing, meaning = [Bidding.OutOfSystem] }
        in
          ElmTest.assertEqual expected (Bidding.annotate testSystem [] (Auction.Bid 3 Nothing))
    ]


chooseSuite : ElmTest.Test
chooseSuite =
  ElmTest.suite "choose"
    [ ElmTest.test "picks one of the bids suggested by the system" <|
        let
          (choice, _) = Bidding.choose testSystem [] [] (Random.initialSeed 0)
        in
          ElmTest.assert (choice == oneNoTrump || choice == twoNoTrump)

    , ElmTest.test "falls back to Pass" <|
        let
          nullSystem =
            { name = "Null System", suggestions = always [] }
          expected =
            { bid = Auction.Pass, meaning = [Bidding.OutOfSystem] }
          (choice, _) =
            Bidding.choose nullSystem [] [] (Random.initialSeed 0)
        in
          ElmTest.assertEqual expected choice
    ]


testSystem : Bidding.System
testSystem =
  { name = "Test System"
  , suggestions = always [oneNoTrump, twoNoTrump]
  }


oneNoTrump : Bidding.AnnotatedBid
oneNoTrump =
  { bid = Auction.Bid 1 Nothing, meaning = [Bidding.HighCardPoints 15 17] }


twoNoTrump : Bidding.AnnotatedBid
twoNoTrump =
  { bid = Auction.Bid 2 Nothing, meaning = [Bidding.HighCardPoints 20 22] }
