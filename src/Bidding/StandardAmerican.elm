module Bidding.StandardAmerican (system) where

{-| This module implements the Standard American Yellow Card bidding system.
-}

import Auction
import Bidding


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
          [ Bidding.HighCardPoints lo hi
          , Bidding.Balanced
          ]
      }
  in
    [ noTrump 1 15 17
    , noTrump 2 20 21
    , noTrump 3 25 27
    ]
