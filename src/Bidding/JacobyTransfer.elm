module Bidding.JacobyTransfer exposing
  ( bids
  )

{-| This module implements the Jacoby Transfer convention.
-}

import Auction
import Bidding
import Card

import Maybe.Extra


{-| Jacoby Transfer bids at the given level.
-}
bids : Int -> Maybe Bidding.Meaning -> List Bidding.AnnotatedBid
bids level extraConditions =
  [ bid level extraConditions Card.Hearts Card.Spades
  , bid level extraConditions Card.Diamonds Card.Hearts
  ]


{-| A Jacoby Transfer bid.
-}
bid : Int -> Maybe Bidding.Meaning -> Card.Suit -> Card.Suit -> Bidding.AnnotatedBid
bid level extraConditions via target =
  let
    baseCondition =
      Bidding.Minimum (Bidding.Length target) (Bidding.Constant 5)
    conditions =
      baseCondition :: Maybe.Extra.maybeToList extraConditions
  in
    { bid = Auction.Bid level (Just via)
    , meaning = Bidding.And conditions
    , description = Just <| "transfer to " ++ toString target
    , convention = Just Bidding.JacobyTransfer
    }
