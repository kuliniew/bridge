module Bidding
  ( System
  , AnnotatedBid
  , Meaning (..)
  , annotate
  , choose
  ) where

{-| This module implements the interface for bidding systems, and provides
common functions used by several different systems.
-}

import Auction
import Card exposing (Card)

import Random
import Random.Extra


{-| A bidding system used by a partnership.  In each scenario, it provides
a list of suggested bids, any of which is considered acceptable by the
system.
-}
type alias System =
  { name : String
  , suggestions : List AnnotatedBid -> List AnnotatedBid
  }


{-| An individual bid, annotated with the significance of the bid.
-}
type alias AnnotatedBid =
  { bid : Auction.Bid
  , meaning : List Meaning
  }


{-| Part of the meaning behind a bid.
-}
type Meaning
  = OutOfSystem
  | HighCardPoints Int Int


{-| Annotate a bid with its meaning in a particular system.
-}
annotate : System -> List AnnotatedBid -> Auction.Bid -> AnnotatedBid
annotate system history bid =
  case lookup bid (system.suggestions history) of
    Just annotated -> annotated
    Nothing -> { bid = bid, meaning = [OutOfSystem] }


{-| Lookup a specific bid in a list of suggestions.
-}
lookup : Auction.Bid -> List AnnotatedBid -> Maybe AnnotatedBid
lookup bid =
  List.filter (\choice -> choice.bid == bid) >> List.head


{-| Choose a bid.  If the system makes more than one suggestion, one of the
suggestions is chosen at random.  If the system makes no suggestion, the
result will be Pass for reason of being OutOfSystem.
-}
choose : System -> List AnnotatedBid -> List Card -> Random.Seed -> (AnnotatedBid, Random.Seed)
choose system history hand =
  let
    fallback = { bid = Auction.Pass, meaning = [OutOfSystem] }
  in
    Random.generate (Random.Extra.selectWithDefault fallback <| system.suggestions history)
