module Auction (Bid (..)) where

{-| This module models the auction phase of a hand of bridge.
-}

import Card


{-| A bid.  Lists of bids are interpreted as most-recent first, oldest
    last.  In other words, new bids are added to the front of the list.
-}
type Bid
  = Pass
  | Double
  | Redouble
  | Bid Int (Maybe Card.Suit)
