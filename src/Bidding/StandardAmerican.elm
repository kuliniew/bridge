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
  [ { bid = Auction.Bid 1 Nothing
    , meaning =
        [ Bidding.HighCardPoints 15 17
        , Bidding.Balanced
        ]
    }
  ]
