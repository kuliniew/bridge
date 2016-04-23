module Card.Producer
  ( suit
  , rank
  , card
  , hand
  ) where

import Card
import Producers

import Array
import Check.Producer exposing (Producer)
import Random
import Random.Array
import Shrink

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


{-| A producer for a complete hand of cards.
-}
hand : Producer (List Card.Card)
hand =
  { generator =
      Random.Array.shuffle Card.deck
        |> Random.map (Array.slice 0 13)
        |> Random.map Array.toList
  , shrinker = Shrink.noShrink
  }
