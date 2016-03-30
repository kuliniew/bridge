module Action (Action (..)) where

import Auction

import Time exposing (Time)


type Action
  = Reseed Time
  | NewDeal
  | Bid Auction.Bid
