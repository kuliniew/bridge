module Card (Suit (..), suits, Rank (..), ranks, Card, deck) where

{-| This module describes a standard deck of playing cards.
-}

import Array exposing (Array)


{-| The suit of a card.
-}
type Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs


{-| Array of all suits.
-}
suits : Array Suit
suits =
  Array.fromList [Spades, Hearts, Diamonds, Clubs]


{-| The rank of a card.
-}
type Rank
  = Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two


{-| Array of all ranks.
-}
ranks : Array Rank
ranks =
  Array.fromList [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]


{-| A playing card.
-}
type alias Card =
  { suit : Suit
  , rank : Rank
  }


{-| A complete deck of playing cards, suitable for shuffling.
-}
deck : Array Card
deck =
  let
    addRanks suit =
      Array.toList ranks
        |> List.map (toCard suit)

    toCard suit rank =
      { suit = suit, rank = rank }
  in
    Array.toList suits
      |> List.concatMap addRanks
      |> Array.fromList
