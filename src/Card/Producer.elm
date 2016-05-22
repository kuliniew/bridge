module Card.Producer exposing
  ( suit
  , rank
  , card
  , hand
  , deal
  )

{-| This module contains elm-check producers for Card.
-}

import Card
import Producers

import Array
import Check.Producer exposing (Producer)
import Random
import Random.Array
import Shrink


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
  let
    head' xs =
      case List.head xs of
        Just x -> x
        Nothing -> Debug.crash "Card.deal was supposed to return 4 hands, but returned 0"
  in
    Check.Producer.map head' deal


{-| A producer for a complete deal of cards.
-}
deal : Producer (List (List Card.Card))
deal =
  { generator = Card.deal
  , shrinker = Shrink.noShrink
  }
