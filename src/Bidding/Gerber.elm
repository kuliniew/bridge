module Bidding.Gerber exposing
  ( askForAces
  , response
  )

{-| This module implements the Gerber convention.

TODO: Also implement asking for Kings, not just Aces.
-}

import Auction
import Bidding
import Card
import Vulnerability

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


{-| Response to a Gerber bid.
-}
response : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> Maybe (List Bidding.AnnotatedBid)
response _ history =
  let
    responses level rank =
      [ { bid = Auction.Bid level (Just Card.Diamonds)
        , meaning = Bidding.Or
            [ Bidding.Equal (Bidding.CountRank rank) (Bidding.Constant 0)
            , Bidding.Equal (Bidding.CountRank rank) (Bidding.Constant 4)
            ]
        , description = Just "0 or 4 aces"
        , convention = Just Bidding.Gerber
        }

      , { bid = Auction.Bid level (Just Card.Hearts)
        , meaning = Bidding.Equal (Bidding.CountRank rank) (Bidding.Constant 1)
        , description = Just "1 ace"
        , convention = Just Bidding.Gerber
        }

      , { bid = Auction.Bid level (Just Card.Spades)
        , meaning = Bidding.Equal (Bidding.CountRank rank) (Bidding.Constant 2)
        , description = Just "2 aces"
        , convention = Just Bidding.Gerber
        }

      , { bid = Auction.Bid level Nothing
        , meaning = Bidding.Equal (Bidding.CountRank rank) (Bidding.Constant 3)
        , description = Just "3 aces"
        , convention = Just Bidding.Gerber
        }
      ]
  in
    case List.map .bid history of
      Auction.Pass :: Auction.Bid 4 (Just Card.Clubs) :: _ -> Just (responses 4 Card.Ace)
      _ -> Nothing


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
