module Auction.Producer exposing (auction)

{-| This module contains elm-check producers for Auction.
-}

import Auction

import Check.Producer exposing (Producer)
import Random
import Random.Extra
import Shrink


{-| A producer for legal (but not necessarily complete) auction
    histories.
-}
auction : Producer (List Auction.Bid)
auction =
  { generator = genAuction
  , shrinker = Shrink.list Shrink.noShrink
  }


{-| Generate a valid sequence of auction bids.
-}
genAuction : Random.Generator (List Auction.Bid)
genAuction =
  Random.int 0 20 `Random.andThen` extendAuction []


{-| Add additional bids to an auction.
-}
extendAuction : List Auction.Bid -> Int -> Random.Generator (List Auction.Bid)
extendAuction history needed =
  if needed > 0 && Auction.isOpen history
    then
      let
        wrapUp chosen =
          case chosen of
            Just bid -> extendAuction (bid :: history) (needed - 1)
            Nothing -> Random.Extra.constant history
      in
        Random.Extra.sample (Auction.legalBids history) `Random.andThen` wrapUp
    else
      Random.Extra.constant history
