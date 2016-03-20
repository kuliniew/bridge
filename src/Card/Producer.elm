module Card.Producer (suit, rank, card) where

import Card

import Array exposing (Array)
import Check.Producer exposing (Producer)

{-| This module contains elm-check producers for Card.
-}


{-| A producer for suits.
-}
suit : Producer Card.Suit
suit =
  elementOf Card.suits


{- | A producer for ranks.
-}
rank : Producer Card.Rank
rank =
  elementOf Card.ranks


{- | A producer for cards.
-}
card : Producer Card.Card
card =
  let
    toCard = \(suit, rank) -> { suit = suit, rank = rank }
  in
    Check.Producer.tuple (suit, rank) |> Check.Producer.map toCard


{- | A producer for elements sampled from an array of choices.
-}
elementOf : Array a -> Producer a
elementOf elements =
  let
    maxIndex =
      Array.length elements - 1

    toElement index =
      case Array.get index elements of
        Just element -> element
        Nothing -> Debug.crash "failed to generate an appropriate index into the array!"
  in
    Check.Producer.map toElement (Check.Producer.rangeInt 0 maxIndex)
