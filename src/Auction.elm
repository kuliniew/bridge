module Auction
  ( Bid (..)
  , isOpen
  , legalBids
  ) where

{-| This module models the auction phase of a hand of bridge.
-}

import Card

import List.Extra


{-| A bid.  Lists of bids are interpreted as most-recent first, oldest
    last.  In other words, new bids are added to the front of the list.
-}
type Bid
  = Pass
  | Double
  | Redouble
  | Bid Int (Maybe Card.Suit)


{-| Check whether the auction is still open.
-}
isOpen : List Bid -> Bool
isOpen auction =
  case auction of
    Pass :: Pass :: Pass :: _ :: _ -> False
    _ -> True


{-| Get a list of legal bids.
-}
legalBids : List Bid -> List Bid
legalBids auction =
  let
    double =
      if canDouble auction then [Double] else []
    redouble =
      if canRedouble auction then [Redouble] else []
  in
    Pass :: double ++ redouble ++ legalContractualBids (latestContractualBid auction)


{-| Return the remaining legal contractual bids, given the current
contractual bid (if any).
-}
legalContractualBids : Maybe Bid -> List Bid
legalContractualBids current =
  case current of
    Just bid -> Maybe.withDefault [] <| List.tail <| List.Extra.dropWhile ((/=) bid) contractualBids
    Nothing -> contractualBids


{-| Check whether Double is a legal bid.
-}
canDouble : List Bid -> Bool
canDouble auction =
  case auction of
    Bid _ _ :: _ -> True
    Pass :: Pass :: Bid _ _ :: _ -> True
    _ -> False


{-| Check whether Redouble is a legal bid.
-}
canRedouble : List Bid -> Bool
canRedouble auction =
  case auction of
    Double :: _ -> True
    Pass :: Pass :: Double :: _ -> True
    _ -> False


{-| Return the most recent bid that would determine the contract, if any.
-}
latestContractualBid : List Bid -> Maybe Bid
latestContractualBid =
  let
    isContractualBid bid =
      case bid of
        Bid _ _ -> True
        _ -> False
  in
    List.head << List.filter isContractualBid


{-| List of all possible contractual bids, in order.
-}
contractualBids : List Bid
contractualBids =
  let
    bidsAtLevel level =
      List.map (Bid level) [Just Card.Clubs, Just Card.Diamonds, Just Card.Hearts, Just Card.Spades, Nothing]
  in
    List.concatMap bidsAtLevel [1 .. 7]
