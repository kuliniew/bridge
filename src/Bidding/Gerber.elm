module Bidding.Gerber exposing
  ( askForAces
  )

{-| This module implements the Gerber convention.

TODO: Also implement asking for Kings, not just Aces.
-}

import Auction
import Bidding
import Card

import Maybe.Extra


{-| Gerber ace-asking bid.
-}
askForAces : Maybe Bidding.Meaning -> Bidding.AnnotatedBid
askForAces extraConditions =
  let
    baseConditions =
      [ noVoids
      , noTwoQuickLosers
      ]
    conditions =
      baseConditions ++ Maybe.Extra.maybeToList extraConditions
  in
    { bid = Auction.Bid 4 (Just Card.Clubs)
    , meaning = Bidding.And conditions
    , description = Just "ask for aces"
    , convention = Just Bidding.Gerber
    }


{-| Require that a hand contain no voids.
-}
noVoids : Bidding.Meaning
noVoids =
  let
    noVoidIn suit =
      Bidding.GreaterThan (Bidding.Length suit) (Bidding.Constant 0)
  in
    Bidding.And <| List.map noVoidIn Card.suits


{-| Require that a hand have no suit with two quick losers.
-}
noTwoQuickLosers : Bidding.Meaning
noTwoQuickLosers =
  let
    noTwoQuickLosersIn suit =
      Bidding.LessThan (Bidding.QuickLosers suit) (Bidding.Constant 2)
  in
    Bidding.And <| List.map noTwoQuickLosersIn Card.suits
