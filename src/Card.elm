module Card
  ( Suit (..)
  , suits
  , Rank (..)
  , ranks
  , Card
  , deck
  , rankDescending
  , SampleHand
  , fromSuits
  ) where

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


{-| Numerical equivalent of a rank.
-}
numericalRank : Rank -> Int
numericalRank rank =
  case rank of
    Ace -> 14
    King -> 13
    Queen -> 12
    Jack -> 11
    Ten -> 10
    Nine -> 9
    Eight -> 8
    Seven -> 7
    Six -> 6
    Five -> 5
    Four -> 4
    Three -> 3
    Two -> 2


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


{-| Comparitor to sort cards in descending order by rank.
-}
rankDescending : Card -> Card -> Order
rankDescending card1 card2 = (numericalRank card2.rank) `compare` (numericalRank card1.rank)


{-| Record used when writing test cases.
-}
type alias SampleHand a =
  { a | spades : List Rank, hearts : List Rank, diamonds : List Rank, clubs : List Rank }


{-| Expand a sample hand into a full list of cards.
-}
fromSuits : SampleHand a -> List Card
fromSuits sample =
  let
    addSuit suit rank =
      { suit = suit, rank = rank }
    cards =
      List.concat
        [ List.map (addSuit Spades) sample.spades
        , List.map (addSuit Hearts) sample.hearts
        , List.map (addSuit Diamonds) sample.diamonds
        , List.map (addSuit Clubs) sample.clubs
        ]
    length = List.length cards
  in
    if length == 13
      then cards
      else Debug.crash ("sanity check: sample hand has " ++ toString length ++ " cards, not 13")
