module Bidding.TestUtils exposing
  ( BidTest
  , testBid
  , suggestionsExist
  , annotate
  )

import Auction
import Bidding
import Card
import Card.Producer
import TestUtils
import Vulnerability
import Vulnerability.Producer

import Check
import Check.Producer
import ElmTest
import List.Extra


type alias BidTest =
  { name : String
  , expected : List Auction.Bid
  , favorability : Vulnerability.Favorability
  , history : List Auction.Bid
  , spades : List Card.Rank
  , hearts : List Card.Rank
  , diamonds : List Card.Rank
  , clubs : List Card.Rank
  }


{-| Test that the expectd bids are suggested in a situation.
-}
testBid : Bidding.System -> BidTest -> ElmTest.Test
testBid system test =
  let
    chosen =
      Bidding.viableChoices system test.favorability (annotate system test.favorability test.history) (Card.fromSuits test)
        |> List.map .bid
    message = "expected " ++ toString (test.expected) ++ " but got " ++ toString (chosen)
  in
    ElmTest.test test.name <|
      if chosen `List.Extra.isPermutationOf` test.expected
        then ElmTest.pass
        else ElmTest.fail message


{-| Test that bids are suggested regardless of the hand or vulnerability.
-}
suggestionsExist : Bidding.System -> List Auction.Bid -> ElmTest.Test
suggestionsExist system history =
  let
    rotate xs =
      case (List.head xs, List.tail xs) of
        (Just h, Just t) -> t ++ [h]
        _ -> Debug.crash "tried to rotate an empty list somehow!"
    head' xs =
      case List.head xs of
        Just x -> x
        Nothing -> Debug.crash "Card.Producer.deal produced an empty list of hands!"
    injectOutOfSystem suggestions =
      if List.isEmpty suggestions
         then [ { bid = Auction.Pass, meaning = Bidding.OutOfSystem, description = Nothing, convention = Nothing } ]
         else suggestions
    consistent favorability annHist hands =
      case annHist of
        [] -> True
        bid :: rest ->
          if List.member bid (injectOutOfSystem <| Bidding.viableChoices system favorability rest (head' hands))
            then consistent favorability rest (rotate hands)
            else False
    plausibleSetup =
      Check.Producer.tuple (Vulnerability.Producer.favorability, Card.Producer.deal)
        |> Check.Producer.filter (\(favorability, hands) -> consistent favorability (annotate system favorability history) (rotate hands))
        |> Check.Producer.map (\(favorability, hands) -> (favorability, head' hands))
    suggestionsExist' favorability hand =
      not <| List.isEmpty <| Bidding.viableChoices system favorability (annotate system favorability history) hand
  in
    TestUtils.generativeTest <|
      Check.claim
        ("bids are always suggested for " ++ toString history)
      `Check.true`
        uncurry suggestionsExist'
      `Check.for`
        plausibleSetup


{-| Annotate an entire bidding history.
-}
annotate : Bidding.System -> Vulnerability.Favorability -> List Auction.Bid -> List Bidding.AnnotatedBid
annotate system favorability history =
  let
    augment (fav, bid) annotatedHistory =
      Bidding.annotate system fav annotatedHistory bid :: annotatedHistory
    tagHistory fav hist =
      case hist of
        [] -> []
        bid :: rest -> (fav, bid) :: tagHistory (Vulnerability.opposing fav) rest
    taggedHistory = tagHistory (Vulnerability.opposing favorability) history
  in
    List.foldr augment [] taggedHistory
