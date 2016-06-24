module BiddingTests exposing (all)

import Auction
import Bidding
import Card
import Vulnerability

import ElmTest
import Random


all : ElmTest.Test
all =
  ElmTest.suite "Bidding"
    [ annotateSuite
    , chooseSuite
    , roleSuite
    ]


annotateSuite : ElmTest.Test
annotateSuite =
  ElmTest.suite "annotate"
    [ ElmTest.test "takes annotations from the system" <|
        ElmTest.assertEqual twoNoTrump (Bidding.annotate testSystem Vulnerability.Equal [] (Auction.Bid 2 Nothing))

    , ElmTest.test "falls back to OutOfSystem" <|
        ElmTest.assertEqual Bidding.OutOfSystem (Bidding.annotate testSystem Vulnerability.Equal [] (Auction.Bid 3 Nothing)).meaning
    ]


chooseSuite : ElmTest.Test
chooseSuite =
  ElmTest.suite "choose"
    [ ElmTest.test "picks one of the bids suggested by the system" <|
        let
          choice = quickGenerate <| Bidding.choose testSystem Vulnerability.Equal [] []
        in
          ElmTest.assert (choice == oneNoTrump || choice == twoNoTrump)

    , ElmTest.test "falls back to Pass" <|
        let
          nullSystem =
            { name = "Null System", suggestions = \_ _ -> [] }
          choice =
            quickGenerate <| Bidding.choose nullSystem Vulnerability.Equal [] []
        in
          ElmTest.assertEqual Auction.Pass choice.bid
    ]


roleSuite : ElmTest.Test
roleSuite =
  ElmTest.suite "role'"
    [ ElmTest.test "first bid at first seat is openable" <|
        let
          history = []
        in
          ElmTest.assertEqual Bidding.Openable (Bidding.role' history)

    , ElmTest.test "first bid at second seat is openable if first seat passed" <|
        let
          history = [Auction.Pass]
        in
          ElmTest.assertEqual Bidding.Openable (Bidding.role' history)

    , ElmTest.test "first bid at third seat is openable if first two seats passed" <|
        let
          history = [Auction.Pass, Auction.Pass]
        in
          ElmTest.assertEqual Bidding.Openable (Bidding.role' history)

    , ElmTest.test "first bid at fourth seat is openable if first three seats passed" <|
        let
          history = [Auction.Pass, Auction.Pass, Auction.Pass]
        in
          ElmTest.assertEqual Bidding.Openable (Bidding.role' history)

    , ElmTest.test "is opener if previously made the first non-Pass bid (first seat)" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass, Auction.Bid 1 (Just Card.Spades)
            ]
        in
          ElmTest.assertEqual Bidding.Opener (Bidding.role' history)

    , ElmTest.test "is opener if previously made the first non-Pass bid (second seat)" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass, Auction.Bid 1 (Just Card.Spades)
            , Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Opener (Bidding.role' history)

    , ElmTest.test "is opener if previously made the first non-Pass bid (third seat)" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass, Auction.Bid 1 (Just Card.Spades)
            , Auction.Pass, Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Opener (Bidding.role' history)

    , ElmTest.test "is opener if previously made the first non-Pass bid (fourth seat)" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass, Auction.Bid 1 (Just Card.Spades)
            , Auction.Pass, Auction.Pass, Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Opener (Bidding.role' history)

    , ElmTest.test "is responder if partner opened in first seat" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds)
            ]
        in
          ElmTest.assertEqual Bidding.Responder (Bidding.role' history)

    , ElmTest.test "is responder if partner opened in second seat" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Responder (Bidding.role' history)

    , ElmTest.test "is responder if partner opened in third seat" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass, Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Responder (Bidding.role' history)

    , ElmTest.test "is responder if partner opened in fourth seat" <|
        let
          history =
            [ Auction.Pass, Auction.Bid 1 (Just Card.Diamonds), Auction.Pass, Auction.Pass
            , Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Responder (Bidding.role' history)

    , ElmTest.test "is defender if right-hand opponent is opener in first seat" <|
        let
          history =
            [ Auction.Bid 1 (Just Card.Clubs)
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if right-hand opponent is opener in second seat" <|
        let
          history =
            [ Auction.Bid 1 (Just Card.Clubs), Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if right-hand opponent is opener in third seat" <|
        let
          history =
            [ Auction.Bid 1 (Just Card.Clubs), Auction.Pass, Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if right-hand opponent is opener in fourth seat" <|
        let
          history =
            [ Auction.Bid 1 (Just Card.Clubs), Auction.Pass, Auction.Pass, Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if left-hand opponent is opener in first seat" <|
        let
          history =
            [ Auction.Pass, Auction.Pass, Auction.Bid 1 (Just Card.Clubs)
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if left-hand opponent is opener in second seat" <|
        let
          history =
            [ Auction.Pass, Auction.Pass, Auction.Bid 1 (Just Card.Clubs), Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if left-hand opponent is opener in third seat" <|
        let
          history =
            [ Auction.Pass, Auction.Pass, Auction.Bid 1 (Just Card.Clubs), Auction.Pass
            , Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)

    , ElmTest.test "is defender if left-hand opponent is opener in fourth seat" <|
        let
          history =
            [ Auction.Pass, Auction.Pass, Auction.Bid 1 (Just Card.Clubs), Auction.Pass
            , Auction.Pass, Auction.Pass
            ]
        in
          ElmTest.assertEqual Bidding.Defender (Bidding.role' history)
    ]


testSystem : Bidding.System
testSystem =
  { name = "Test System"
  , suggestions = \_ _ -> [oneNoTrump, twoNoTrump]
  }


oneNoTrump : Bidding.AnnotatedBid
oneNoTrump =
  { bid = Auction.Bid 1 Nothing
  , description = Nothing
  , convention = Nothing
  , meaning = nop
  }


twoNoTrump : Bidding.AnnotatedBid
twoNoTrump =
  { bid = Auction.Bid 2 Nothing
  , description = Nothing
  , convention = Nothing
  , meaning = nop
  }


nop : Bidding.Meaning
nop = Bidding.Minimum (Bidding.Constant 0) (Bidding.Constant 0)


quickGenerate : Random.Generator a -> a
quickGenerate generator =
  fst <| Random.step generator (Random.initialSeed 0)
