module Evaluation
  ( highCardPoints
  , lengthPoints
  , shortnessPoints
  , points

  , length
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


{-| Count the length points in a hand.
-}
lengthPoints : List Card -> Int
lengthPoints cards =
  List.map (\suit -> length suit cards - 4) suits
    |> List.filter (\pts -> pts > 0)
    |> List.sum


{-| Count the shortness points in a hand.
-}
shortnessPoints : Card.Suit -> List Card -> Int
shortnessPoints trump cards =
  List.filter (\suit -> suit /= trump) suits
    |> List.map (\suit -> 3 - suitLength cards suit)
    |> List.filter (\pts -> pts > 0)
    |> List.sum


{-| Count the total points in a hand, given knowledge of trumps.
-}
points : Maybe (Maybe Card.Suit) -> List Card -> Int
points trump cards =
  case trump of
    Nothing -> highCardPoints cards + lengthPoints cards
    Just Nothing -> highCardPoints cards
    Just (Just suit) -> highCardPoints cards + shortnessPoints suit cards


{-| Measure the length of a particular suit.
-}
length : Card.Suit -> List Card -> Int
length suit cards =
  List.length <| List.filter (\card -> card.suit == suit) cards


{-| Measure the distribution of cards across suits.
-}
distribution : List Card -> List Int
distribution cards =
  List.sortBy negate <| List.map (flip length cards) suits


{-| Check if a distribution is balanced.
-}
balanced : List Int -> Bool
balanced dist =
  case dist of
    [4, 4, 3, 2] -> True
    [4, 3, 3, 3] -> True
    [5, 3, 3, 2] -> True
    _ -> False


{-| List of all suits.
-}
suits : List Card.Suit
suits =
  [ Card.Spades, Card.Hearts, Card.Diamonds, Card.Clubs ]
