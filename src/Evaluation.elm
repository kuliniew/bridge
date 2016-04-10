module Evaluation
  ( highCardPoints
  , lengthPoints
  , shortnessPoints
  , points

  , length
  , distribution
  , balanced

  , playingTricks
  , playingTricksAny
  ) where

{-| This module implements various hand evaluation functions.
-}

import Auction
import Card exposing (Card)

import List.Extra


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
    |> List.map (\suit -> 3 - length suit cards)
    |> List.filter (\pts -> pts > 0)
    |> List.sum


{-| Count the total points in a hand, given knowledge of trumps.
-}
points : Maybe Auction.Trump -> List Card -> Int
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


{-| Count the playing tricks in a hand, given a trump suit.
-}
playingTricks : Auction.Trump -> List Card -> Int
playingTricks trump cards =
  let
    suitTricks suit =
      let
        ranks =
          List.filter (\card -> card.suit == suit) cards
            |> List.sortWith Card.rankDescending
            |> List.map .rank
      in
        if trump == Just suit
          then trumpTricks ranks
          else offTrumpTricks ranks

    trumpTricks ranks =
      let
        rankTrick rank =
          if List.member rank ranks then 1 else 0
        lengthTricks =
          max (List.length ranks - 3) 0
      in
        rankTrick Card.Ace + rankTrick Card.King + rankTrick Card.Queen + lengthTricks

    offTrumpTricks ranks =
      let
        ranksFromKing =
          [Card.King, Card.Queen, Card.Jack, Card.Ten, Card.Nine, Card.Eight, Card.Seven, Card.Six, Card.Five, Card.Four, Card.Three, Card.Two]
        ranksFromAce =
          Card.Ace :: ranksFromKing
        matches goals =
          List.map2 (==) ranks goals
            |> List.Extra.takeWhile identity
            |> List.length
        fromAce =
          matches ranksFromAce
        fromKing =
          max (matches ranksFromKing - 1) 0
      in
        fromAce + fromKing
  in
    List.sum <| List.map suitTricks suits


{-| Count the playing tricks in a hand, for whichever the best
trump suit may be.
-}
playingTricksAny : List Card -> Int
playingTricksAny cards =
  [Card.Spades, Card.Hearts, Card.Diamonds, Card.Clubs]
    |> List.map (\suit -> playingTricks (Just suit) cards)
    |> List.maximum
    |> Maybe.withDefault 0
