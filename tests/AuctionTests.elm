module AuctionTests exposing (all)

import Auction
import Auction.Producer
import Card
import TestUtils

import Check
import Check.Producer
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Auction"
    [ legalBidsSuite
    , auctionOpen
    ]


legalBidsSuite : ElmTest.Test
legalBidsSuite =
  ElmTest.suite "legalBids"
    [ TestUtils.generativeTest <|
        Check.claim
          "Pass is always a legal bid"
        `Check.true`
          (\auction -> List.member Auction.Pass (Auction.legalBids auction))
        `Check.for`
          Auction.Producer.auction

    , TestUtils.generativeTest <|
        Check.claim
          "Only bids of level 1-7 are ever legal"
        `Check.true`
          (
            let
              wellFormed bid =
                case bid of
                  Auction.Pass -> True
                  Auction.Double -> True
                  Auction.Redouble -> True
                  Auction.Bid level _ -> 1 <= level && level <= 7
            in
              List.all wellFormed
          )
        `Check.for`
          Auction.Producer.auction

    , ElmTest.test "The opening bid can be Passed or at any level" <|
        let
          auction = []
          expected = Auction.Pass :: fullLevels [1 .. 7]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "Clubs bids can be outbid by Diamonds, Hearts, Spades, and No Trump at the same level" <|
        let
          auction = [Auction.Bid 1 (Just Card.Clubs)]
          sameLevel = List.map (Auction.Bid 1) [Just Card.Diamonds, Just Card.Hearts, Just Card.Spades, Nothing]
          expected = Auction.Pass :: Auction.Double :: sameLevel ++ fullLevels [2 .. 7]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "Diamonds bids can be outbid by Hearts, Spades, and No Trump at the same level" <|
        let
          auction = [Auction.Bid 1 (Just Card.Diamonds)]
          sameLevel = List.map (Auction.Bid 1) [Just Card.Hearts, Just Card.Spades, Nothing]
          expected = Auction.Pass :: Auction.Double :: sameLevel ++ fullLevels [2 .. 7]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "Hearts bids can be outbid by Spades and No Trump at the same level" <|
        let
          auction = [Auction.Bid 1 (Just Card.Hearts)]
          sameLevel = List.map (Auction.Bid 1) [Just Card.Spades, Nothing]
          expected = Auction.Pass :: Auction.Double :: sameLevel ++ fullLevels [2 .. 7]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "Spades bids can be outbid by No Trump at the same level" <|
        let
          auction = [Auction.Bid 1 (Just Card.Spades)]
          sameLevel = Auction.Bid 1 Nothing
          expected = Auction.Pass :: Auction.Double :: sameLevel :: fullLevels [2 .. 7]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "No-trump bids can only be outbid at a higher level" <|
        let
          auction = [Auction.Bid 1 Nothing]
          expected = Auction.Pass :: Auction.Double :: fullLevels [2 .. 7]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "No contractual bids remain after 7NT" <|
        let
          auction = [Auction.Bid 7 Nothing]
          expected = [Auction.Pass, Auction.Double]
        in
          ElmTest.assertEqual expected (Auction.legalBids auction)

    , ElmTest.test "Can Double the right-hand opponent's bid" <|
        let
          auction = [Auction.Bid 1 Nothing]
        in
          ElmTest.assert (canDouble auction)

    , ElmTest.test "Cannot Double the right-hand opponent's Double" <|
        let
          auction = [Auction.Double, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canDouble auction)

    , ElmTest.test "Cannot Double partner's bid" <|
        let
          auction = [Auction.Pass, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canDouble auction)

    , ElmTest.test "Cannot Double partner's Double" <|
        let
          auction = [Auction.Pass, Auction.Double, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canDouble auction)

    , ElmTest.test "Can Double the left-hand opponent's bid" <|
        let
          auction = [Auction.Pass, Auction.Pass, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (canDouble auction)

    , ElmTest.test "Cannot Double the left-hand opponent's Double" <|
        let
          auction = [Auction.Pass, Auction.Pass, Auction.Double, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canDouble auction)

    , ElmTest.test "Can Redouble the right-hand opponent's Double" <|
        let
          auction = [Auction.Double, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (canRedouble auction)

    , ElmTest.test "Cannot Redouble the right-hand opponent's bid" <|
        let
          auction = [Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canRedouble auction)

    , ElmTest.test "Cannot Redouble partner's Double" <|
        let
          auction = [Auction.Pass, Auction.Double, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canRedouble auction)

    , ElmTest.test "Cannot Redouble partner's bid" <|
        let
          auction = [Auction.Pass, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canRedouble auction)

    , ElmTest.test "Can Redouble the left-hand opponent's Double" <|
        let
          auction = [Auction.Pass, Auction.Pass, Auction.Double, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (canRedouble auction)

    , ElmTest.test "Cannot Redouble the left-hand opponent's bid" <|
        let
          auction = [Auction.Pass, Auction.Pass, Auction.Bid 1 Nothing]
        in
          ElmTest.assert (not <| canRedouble auction)
    ]


auctionOpen : ElmTest.Test
auctionOpen =
  ElmTest.suite "isOpen"
    [ TestUtils.generativeTest <|
        Check.claim
          "Auctions with three or fewer bids are always open"
        `Check.true`
          Auction.isOpen
        `Check.for`
          Check.Producer.filter
            (\auction -> List.length auction <= 3)
            Auction.Producer.auction

    , ElmTest.test "Bidding is open even if the only three bids have been Pass" <|
        ElmTest.assert <| Auction.isOpen [Auction.Pass, Auction.Pass, Auction.Pass]

    , ElmTest.test "Bidding is closed if everyone Passed out" <|
        ElmTest.assert <| not <| Auction.isOpen [Auction.Pass, Auction.Pass, Auction.Pass, Auction.Pass]

    , ElmTest.test "Bidding is open if the last bid is not Pass" <|
        ElmTest.assert <| Auction.isOpen [Auction.Bid 1 Nothing, Auction.Pass, Auction.Pass, Auction.Pass]

    , ElmTest.test "Bidding is open if only the last bid is Pass" <|
        ElmTest.assert <| Auction.isOpen [Auction.Pass, Auction.Bid 1 Nothing, Auction.Pass, Auction.Pass]

    , ElmTest.test "Bidding is open if only the last two bids are Pass" <|
        ElmTest.assert <| Auction.isOpen [Auction.Pass, Auction.Pass, Auction.Bid 1 Nothing, Auction.Pass]

    , ElmTest.test "Bidding is closed if the last three bids are Pass (and everyone has had a chance to bid)" <|
        ElmTest.assert <| not <| Auction.isOpen [Auction.Pass, Auction.Pass, Auction.Pass, Auction.Bid 1 Nothing]
    ]


fullLevels : List Int -> List Auction.Bid
fullLevels =
  let
    fullLevel level = List.map (Auction.Bid level) [Just Card.Clubs, Just Card.Diamonds, Just Card.Hearts, Just Card.Spades, Nothing]
  in
    List.concatMap fullLevel


canDouble : List Auction.Bid -> Bool
canDouble auction = Auction.legalBids auction |> List.member Auction.Double


canRedouble : List Auction.Bid -> Bool
canRedouble auction = Auction.legalBids auction |> List.member Auction.Redouble
