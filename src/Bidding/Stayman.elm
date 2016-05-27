module Bidding.Stayman exposing
  ( bid
  , response
  )

{-| This module implements the Stayman convention.
-}

import Auction
import Bidding
import Card exposing (Suit (Clubs))
import Vulnerability

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


{-| Respond to a Stayman bid.
-}
response : Vulnerability.Favorability -> List Bidding.AnnotatedBid -> Maybe (List Bidding.AnnotatedBid)
response _ history =
  let
    deny suit =
      Bidding.LessThan (Bidding.Length suit) (Bidding.Constant 4)
    show suit =
      Bidding.Minimum (Bidding.Length suit) (Bidding.Constant 4)
    negative level =
      { bid = Auction.Bid level (Just Card.Diamonds)
      , meaning = Bidding.And [deny Card.Hearts, deny Card.Spades]
      , description = Just "deny four-card majors"
      , convention = Just Bidding.Stayman
      }
    hearts level =
      { bid = Auction.Bid level (Just Card.Hearts)
      , meaning = show Card.Hearts
      , description = Just "show four or more hearts"
      , convention = Just Bidding.Stayman
      }
    spades level =
      { bid = Auction.Bid level (Just Card.Spades)
      , meaning = Bidding.And [deny Card.Hearts, show Card.Spades]
      , description = Just "deny four hearts, show four or more spades"
      , convention = Just Bidding.Stayman
      }
    responses level =
      [ negative level, hearts level, spades level ]
  in
    {- the compiler (at least 0.16?) doesn't accept "Auction.Bid level (Just Card.Clubs)" below -}
    case List.map .bid history of
      Auction.Pass :: Auction.Bid level (Just Clubs) :: _ -> Just (responses level)
      _ -> Nothing


{-| Deny having 4-3-3-3 distribution.
-}
notFourThreeThreeThree : Bidding.Meaning
notFourThreeThreeThree =
  Bidding.Or <| List.map (\suit -> Bidding.Maximum (Bidding.Length suit) (Bidding.Constant 2)) Card.suits
