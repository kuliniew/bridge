module Bidding.Stayman
  ( bid
  ) where

{-| This module implements the Stayman convention.
-}

import Auction
import Bidding
import Card

import Maybe.Extra


{-| Stayman bid at the given level.
-}
bid : Int -> Maybe Bidding.Meaning -> Bidding.AnnotatedBid
bid level extraConditions =
  let
    baseConditions =
      [ Bidding.Or
        [ Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Constant 4)
        , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Constant 4)
        ]
      , Bidding.Or
          [ Bidding.LessThan (Bidding.Length Card.Spades) (Bidding.Constant 5)
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Constant 4)
          ]
      , Bidding.Or
          [ Bidding.LessThan (Bidding.Length Card.Hearts) (Bidding.Constant 5)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Constant 4)
          ]
      , notFourThreeThreeThree
      ]
    conditions =
      baseConditions ++ Maybe.Extra.maybeToList extraConditions
  in
    { bid = Auction.Bid level (Just Card.Clubs)
    , meaning = Bidding.And conditions
    , description = Just "ask for four-card major"
    , convention = Just Bidding.Stayman
    }


{-| Deny having 4-3-3-3 distribution.
-}
notFourThreeThreeThree : Bidding.Meaning
notFourThreeThreeThree =
  Bidding.Or <| List.map (\suit -> Bidding.Maximum (Bidding.Length suit) (Bidding.Constant 2)) suits


{-| List of all suits.
-}
suits : List Card.Suit
suits = [ Card.Spades, Card.Hearts, Card.Diamonds, Card.Clubs ]
