module Bidding exposing
  ( System
  , Convention (..)
  , AnnotatedBid
  , Meaning (..)
  , Metric (..)
  , annotate
  , choose
  , viableChoices

  , Role (..)
  , role
  , role'
  )

{-| This module implements the interface for bidding systems, and provides
common functions used by several different systems.
-}

import Auction
import Card exposing (Card)
import Evaluation
import Vulnerability

import List.Extra
import Random
import Random.Extra


{-| A bidding system used by a partnership.  In each scenario, it provides
a list of suggested bids, any of which is considered acceptable by the
system.
-}
type alias System =
  { name : String
  , suggestions : Vulnerability.Favorability -> List AnnotatedBid -> List AnnotatedBid
  }


{-| Identifier of a bidding convention.
-}
type Convention
  = Gerber
  | JacobyTransfer
  | Stayman


{-| An individual bid, annotated with the significance of the bid.
-}
type alias AnnotatedBid =
  { bid : Auction.Bid
  , meaning : Meaning
  , description : Maybe String
  , convention : Maybe Convention
  }


{-| Part of the meaning behind a bid.
-}
type Meaning
  = OutOfSystem
  | Forced
  | InRange Metric Int Int
  | Equal Metric Metric
  | Minimum Metric Metric
  | Maximum Metric Metric
  | GreaterThan Metric Metric
  | LessThan Metric Metric
  | Balanced
  | SemiBalanced
  | Or (List Meaning)
  | And (List Meaning)
  | Deny Meaning


{-| A thing that can be measured in a hand.
-}
type Metric
  = Constant Int
  | HighCardPoints
  | Points (Maybe Auction.Trump)
  | Length Card.Suit
  | PlayingTricks
  | QuickLosers Card.Suit


{-| Annotate a bid with its meaning in a particular system.
-}
annotate : System -> Vulnerability.Favorability -> List AnnotatedBid -> Auction.Bid -> AnnotatedBid
annotate system favorability history bid =
  case lookup bid (system.suggestions favorability history) of
    Just annotated -> annotated
    Nothing -> outOfSystem bid


{-| Lookup a specific bid in a list of suggestions.
-}
lookup : Auction.Bid -> List AnnotatedBid -> Maybe AnnotatedBid
lookup bid =
  List.filter (\choice -> choice.bid == bid) >> List.head


{-| Choose a bid.  If the system makes more than one suggestion, one of the
suggestions is chosen at random.  If the system makes no suggestion, the
result will be Pass for reason of being OutOfSystem.
-}
choose : System -> Vulnerability.Favorability -> List AnnotatedBid -> List Card -> Random.Generator AnnotatedBid
choose system favorability history hand =
  let
    fallback = outOfSystem Auction.Pass
  in
    Random.Extra.selectWithDefault fallback <| viableChoices system favorability history hand


{-| Annotate a bid outside of the bidding system.
-}
outOfSystem : Auction.Bid -> AnnotatedBid
outOfSystem bid =
  { bid = bid
  , meaning = OutOfSystem
  , description = Nothing
  , convention = Nothing
  }


{-| Get a list of viable suggested bids, based on the contents of
the bidder's hand.
-}
viableChoices : System -> Vulnerability.Favorability -> List AnnotatedBid -> List Card -> List AnnotatedBid
viableChoices system favorability history hand =
  List.filter (satisfiedBy hand) <| system.suggestions favorability history


{-| Check if a hand satisfies the meaning of a proposed bid.
-}
satisfiedBy : List Card -> AnnotatedBid -> Bool
satisfiedBy hand bid =
  let
    satisfies meaning =
      case meaning of
        OutOfSystem -> True
        Forced -> True
        Equal metric val ->
          eval metric hand == eval val hand
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
        Balanced ->
          Evaluation.balanced (Evaluation.distribution hand)
        SemiBalanced ->
          Evaluation.semiBalanced (Evaluation.distribution hand)
        Or alternatives ->
          List.any satisfies alternatives
        And requirements ->
          List.all satisfies requirements
        Deny prohibition ->
          not (satisfies prohibition)
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
    QuickLosers suit -> Evaluation.quickLosers suit hand


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
