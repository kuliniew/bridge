module Card exposing
  ( Suit (..)
  , suits
  , Rank (..)
  , ranks
  , Card
  , deck
  , deal
  , rankDescending
  , SampleHand
  , fromSuits
  )

{-| This module describes a standard deck of playing cards.
-}

import Array exposing (Array)
import Random
import Random.Array


{-| The suit of a card.
-}
type Suit
  = Spades
  | Hearts
  | Diamonds
  | Clubs


{-| List of all suits.
-}
suits : List Suit
suits =
  [Spades, Hearts, Diamonds, Clubs]


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


{-| List of all ranks.
-}
ranks : List Rank
ranks =
  [Ace, King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two]


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
      List.map (toCard suit) ranks

    toCard suit rank =
      { suit = suit, rank = rank }
  in
    suits
      |> List.concatMap addRanks
      |> Array.fromList


{-| Deal the deck of cards for a game of bridge.
-}
deal : Random.Generator (List (List Card))
deal =
  let
    cardsPerHand =
      Array.length deck // 4
    takeCards shuffled n =
      Array.toList (Array.slice (cardsPerHand * n) (cardsPerHand * (n + 1)) shuffled)
  in
    Random.Array.shuffle deck
      |> Random.map (\shuffled -> List.map (takeCards shuffled) [0 .. 3])


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
