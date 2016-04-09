module Bidding.StandardAmerican (system) where

{-| This module implements the Standard American Yellow Card bidding system.
-}

import Auction
import Bidding
import Card


{-| The bidding system itself.
-}
system : Bidding.System
system =
  { name = "Standard American Yellow Card"
  , suggestions = suggest
  }


{-| The suggestion function.
-}
suggest : List Bidding.AnnotatedBid -> List Bidding.AnnotatedBid
suggest history =
  case Bidding.role history of
    Bidding.Openable -> openingBids
    _ -> []


{-| Possible opening bids.
-}
openingBids : List Bidding.AnnotatedBid
openingBids =
  let
    noTrump level lo hi =
      { bid = Auction.Bid level Nothing
      , meaning =
          [ Bidding.InRange Bidding.HighCardPoints lo hi
          , Bidding.Balanced
          ]
      }
    majorLength = 5
    minorLength = 3
    strongPoints = 23
    oneLevelMinimumPoints =
      Bidding.Minimum (Bidding.Points Nothing) (Bidding.Constant 13)
    oneLevelMaximumPoints =
      Bidding.LessThan Bidding.HighCardPoints (Bidding.Constant strongPoints)
    openWithMinor minor major =
      Bidding.Or
        [ Bidding.LessThan (Bidding.Length major) (Bidding.Constant majorLength)
        , Bidding.GreaterThan (Bidding.Length minor) (Bidding.Length major)
        ]
    oneSpades =
      { bid = Auction.Bid 1 (Just Card.Spades)
      , meaning =
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Constant majorLength)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Length Card.Hearts)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Length Card.Diamonds)
          , Bidding.Minimum (Bidding.Length Card.Spades) (Bidding.Length Card.Clubs)
          ]
      }
    oneHearts =
      { bid = Auction.Bid 1 (Just Card.Hearts)
      , meaning =
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Constant majorLength)
          , Bidding.GreaterThan (Bidding.Length Card.Hearts) (Bidding.Length Card.Spades)
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Length Card.Diamonds)
          , Bidding.Minimum (Bidding.Length Card.Hearts) (Bidding.Length Card.Clubs)
          ]
      }
    oneDiamonds =
      { bid = Auction.Bid 1 (Just Card.Diamonds)
      , meaning =
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Constant minorLength)
          , openWithMinor Card.Diamonds Card.Spades
          , openWithMinor Card.Diamonds Card.Hearts
          , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Length Card.Clubs)
          , Bidding.Or     -- 4-4 goes to diamonds, but 3-3 goes to clubs
              [ Bidding.GreaterThan (Bidding.Length Card.Diamonds) (Bidding.Length Card.Clubs)
              , Bidding.Minimum (Bidding.Length Card.Diamonds) (Bidding.Constant <| minorLength + 1)
              ]
          ]
      }
    oneClubs =
      { bid = Auction.Bid 1 (Just Card.Clubs)
      , meaning =
          [ oneLevelMinimumPoints
          , oneLevelMaximumPoints
          , Bidding.Minimum (Bidding.Length Card.Clubs) (Bidding.Constant minorLength)
          , openWithMinor Card.Clubs Card.Spades
          , openWithMinor Card.Clubs Card.Hearts
          , Bidding.Minimum (Bidding.Length Card.Clubs) (Bidding.Length Card.Diamonds)
          , Bidding.Or     -- 4-4 goes to diamonds, but 3-3 goes to clubs
              [ Bidding.GreaterThan (Bidding.Length Card.Clubs) (Bidding.Length Card.Diamonds)
              , Bidding.Maximum (Bidding.Length Card.Diamonds) (Bidding.Constant minorLength)
              ]
          ]
      }
  in
    [ noTrump 1 15 17
    , noTrump 2 20 21
    , noTrump 3 25 27
    , oneSpades
    , oneHearts
    , oneDiamonds
    , oneClubs
    ]
