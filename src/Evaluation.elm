module Evaluation
  ( highCardPoints

  , distribution
  , balanced
  ) where

{-| This module implements various hand evaluation functions.
-}

import Card exposing (Card)


{-| Count the high-card points (HCP) in a hand.
-}
highCardPoints : List Card -> Int
highCardPoints =
  let
    pointsForCard card =
      case card.rank of
        Card.Ace -> 4
        Card.King -> 3
        Card.Queen -> 2
        Card.Jack -> 1
        _ -> 0
  in
     List.sum << List.map pointsForCard


{-| Measure the distribution of cards across suits.
-}
distribution : List Card -> List Int
distribution cards =
  let
    filterSuit suit = List.filter (\card -> card.suit == suit)
    spades = filterSuit Card.Spades cards
    hearts = filterSuit Card.Hearts cards
    diamonds = filterSuit Card.Diamonds cards
    clubs = filterSuit Card.Clubs cards
  in
    List.sortBy negate <| List.map List.length [spades, hearts, diamonds, clubs]


{-| Check if a distribution is balanced.
-}
balanced : List Int -> Bool
balanced dist =
  case dist of
    [4, 4, 3, 2] -> True
    [4, 3, 3, 3] -> True
    [5, 3, 3, 2] -> True
    _ -> False
