module Bidding.JacobyTransfer exposing
  ( bids
  , response
  )

{-| This module implements the Jacoby Transfer convention.
-}

import Auction
import Bidding
import Card
import Vulnerability

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


{-| Respond to a Jacoby Transfer bid.
-}
response : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> Maybe (List Bidding.AnnotatedBid)
response _ history =
  let
    onlyAccept level target =
      [ { bid = Auction.Bid level (Just target)
        , meaning = Bidding.Forced
        , description = Just "accept"
        , convention = Just Bidding.JacobyTransfer
        }
      ]
    maySuperaccept target =
      [ { bid = Auction.Bid 2 (Just target)
        , meaning = Bidding.Or
            [ Bidding.LessThan (Bidding.Length target) (Bidding.Constant 4)
            , Bidding.LessThan (Bidding.Points (Just <| Just target)) (Bidding.Constant 17)
            ]
        , description = Just "accept"
        , convention = Just Bidding.JacobyTransfer
        }

      , { bid = Auction.Bid 3 (Just target)
        , meaning = Bidding.And
            [ Bidding.Minimum (Bidding.Length target) (Bidding.Constant 4)
            , Bidding.Minimum (Bidding.Points (Just <| Just target)) (Bidding.Constant 17)
            ]
        , description = Just "superaccept"
        , convention = Just Bidding.JacobyTransfer
        }
      ]
    responsesFor level target =
      if level == 2
        then maySuperaccept target
        else onlyAccept level target
  in
    case List.map .bid history of
      Auction.Pass :: Auction.Bid level (Just Card.Hearts) :: _ -> Just (responsesFor level Card.Spades)
      Auction.Pass :: Auction.Bid level (Just Card.Diamonds) :: _ -> Just (responsesFor level Card.Hearts)
      _ -> Nothing
