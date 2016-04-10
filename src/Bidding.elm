module Bidding
  ( System
  , AnnotatedBid
  , Meaning (..)
  , Metric (..)
  , annotate
  , choose
  , viableChoices

  , Role (..)
  , role
  , role'
  ) where

{-| This module implements the interface for bidding systems, and provides
common functions used by several different systems.
-}

import Auction
import Card exposing (Card)
import Evaluation

import List.Extra
import Random
import Random.Extra


{-| A bidding system used by a partnership.  In each scenario, it provides
a list of suggested bids, any of which is considered acceptable by the
system.
-}
type alias System =
  { name : String
  , suggestions : List AnnotatedBid -> List AnnotatedBid
  }


{-| An individual bid, annotated with the significance of the bid.
-}
type alias AnnotatedBid =
  { bid : Auction.Bid
  , meaning : Meaning
  }


{-| Part of the meaning behind a bid.
-}
type Meaning
  = OutOfSystem
  | InRange Metric Int Int
  | Minimum Metric Metric
  | Maximum Metric Metric
  | GreaterThan Metric Metric
  | LessThan Metric Metric
  | Balanced
  | Or (List Meaning)
  | And (List Meaning)
  | NoneOf (List Meaning)


{-| A thing that can be measured in a hand.
-}
type Metric
  = Constant Int
  | HighCardPoints
  | Points (Maybe Auction.Trump)
  | Length Card.Suit
  | PlayingTricks


{-| Annotate a bid with its meaning in a particular system.
-}
annotate : System -> List AnnotatedBid -> Auction.Bid -> AnnotatedBid
annotate system history bid =
  case lookup bid (system.suggestions history) of
    Just annotated -> annotated
    Nothing -> { bid = bid, meaning = OutOfSystem }


{-| Lookup a specific bid in a list of suggestions.
-}
lookup : Auction.Bid -> List AnnotatedBid -> Maybe AnnotatedBid
lookup bid =
  List.filter (\choice -> choice.bid == bid) >> List.head


{-| Choose a bid.  If the system makes more than one suggestion, one of the
suggestions is chosen at random.  If the system makes no suggestion, the
result will be Pass for reason of being OutOfSystem.
-}
choose : System -> List AnnotatedBid -> List Card -> Random.Seed -> (AnnotatedBid, Random.Seed)
choose system history hand =
  let
    fallback = { bid = Auction.Pass, meaning = OutOfSystem }
  in
    Random.generate (Random.Extra.selectWithDefault fallback <| viableChoices system history hand)


{-| Get a list of viable suggested bids, based on the contents of
the bidder's hand.
-}
viableChoices : System -> List AnnotatedBid -> List Card -> List AnnotatedBid
viableChoices system history hand =
  List.filter (satisfiedBy hand) <| system.suggestions history


{-| Check if a hand satisfies the meaning of a proposed bid.
-}
satisfiedBy : List Card -> AnnotatedBid -> Bool
satisfiedBy hand bid =
  let
    satisfies meaning =
      case meaning of
        OutOfSystem -> True
        InRange metric lo hi ->
          let
            value = eval metric hand
          in
            lo <= value && value <= hi
        Minimum metric lo ->
          eval lo hand <= eval metric hand
        Maximum metric hi ->
          eval metric hand <= eval hi hand
        GreaterThan metric below ->
          eval below hand < eval metric hand
        LessThan metric above ->
          eval metric hand < eval above hand
        Balanced -> Evaluation.balanced (Evaluation.distribution hand)
        Or alternatives ->
          List.any satisfies alternatives
        And requirements ->
          List.all satisfies requirements
        NoneOf prohibitions ->
          not (List.any satisfies prohibitions)
  in
    satisfies bid.meaning


{-| Evaluate a metric for a hand.
-}
eval : Metric -> List Card -> Int
eval metric hand =
  case metric of
    Constant value -> value
    HighCardPoints -> Evaluation.highCardPoints hand
    Points trump -> Evaluation.points trump hand
    Length suit -> Evaluation.length suit hand
    PlayingTricks -> Evaluation.playingTricksAny hand


{-| A role that a seat can have during bidding, depending on what bids have
been made so far.
-}
type Role
  = Openable
  | Opener
  | Responder
  | Defender    -- XXX: Should probably also have roles for overcaller and overcaller's partner?


{-| Determine the role of the next player to bid.
-}
role : List AnnotatedBid -> Role
role = role' << List.map .bid


{-| Determine the role of the next player to bid.
-}
role' : List Auction.Bid -> Role
role' history =
  let
    bidsFromOpening = List.reverse history |> List.Extra.dropWhile (\bid -> bid == Auction.Pass) |> List.length
  in
    case bidsFromOpening % 4 of
      0 -> if bidsFromOpening == 0 then Openable else Opener
      1 -> Defender
      2 -> Responder
      3 -> Defender
      _ -> Debug.crash ("bidsFromOpening % 4 wound up being " ++ toString bidsFromOpening ++ " somehow!")