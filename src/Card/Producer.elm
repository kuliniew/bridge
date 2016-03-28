module Card.Producer (suit, rank, card) where

import Card
import Producers

import Check.Producer exposing (Producer)

{-| This module contains elm-check producers for Card.
-}


{-| A producer for suits.
-}
suit : Producer Card.Suit
suit =
  Producers.elementOf Card.suits


{- | A producer for ranks.
-}
rank : Producer Card.Rank
rank =
  Producers.elementOf Card.ranks


{- | A producer for cards.
-}
card : Producer Card.Card
card =
  let
    toCard = \(suit, rank) -> { suit = suit, rank = rank }
  in
    Check.Producer.tuple (suit, rank) |> Check.Producer.map toCard
