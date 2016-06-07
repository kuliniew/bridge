module Knowledge exposing
  ( Metric (..)
  , Knowledge
  , create
  , get
  )

{-| Representation of a player's knowledge of what is in each player's
hand.
-}

import Card
import Constraint
import Evaluation
import Seat


{-| A metric used to evaluate a player's hand.
-}
type Metric
  = HighCardPoints
  | LengthPoints
  | UncommittedPoints
  | ShortnessPointsWithTrump Card.Suit
  | PointsWithTrump Card.Suit
  | Length Card.Suit
  | CountRank Card.Rank
  | PlayingTricks
  | QuickLosers Card.Suit


{-| The variable used in the player's knowledge representation.
-}
type Variable =
  Var Seat.Relative Metric


{-| Representation of a player's total knowledge.
-}
type alias Knowledge =
  Constraint.State Variable


{-| The unconstrained variables composing the knowledge state.
-}
baseKnowledge : Knowledge
baseKnowledge =
  let
    basicMetrics =
      [ (HighCardPoints, Constraint.range 0 40)
      , (LengthPoints, Constraint.range 0 9)
      , (UncommittedPoints, Constraint.range 0 49)
      , (PlayingTricks, Constraint.range 0 13)
      ]
    suitMetrics =
      [ (ShortnessPointsWithTrump, Constraint.range 0 9)
      , (PointsWithTrump, Constraint.range 0 9)
      , (Length, Constraint.range 0 13)
      , (QuickLosers, Constraint.range 0 13)
      ]
    rankMetrics =
      [ (CountRank, Constraint.range 0 4)
      ]
    metrics =
      List.concat
        [ basicMetrics
        , crossMap (\(ctor, range) suit -> (ctor suit, range)) suitMetrics Card.suits
        , crossMap (\(ctor, range) rank -> (ctor rank, range)) rankMetrics Card.ranks
        ]
    variables =
      crossMap (\seat (metric, range) -> (Var seat metric, range)) Seat.relatives metrics
  in
    Constraint.initialize variables


{-| Map over the cross product of two lists.
-}
crossMap : (a -> b -> c) -> List a -> List b -> List c
crossMap f xs ys =
  List.concatMap (\x -> List.map (f x) ys) xs


{-| Create the initial knowledge given a single hand.
-}
create : List Card.Card -> Knowledge
create hand =
  let
    suitConstraint suit =
      Constraint.Equal
        (Constraint.Variable <| Var Seat.Self (Length suit))
        (Constraint.Constant <| Evaluation.length suit hand)
    rankConstraint rank =
      Constraint.Equal
        (Constraint.Variable <| Var Seat.Self (CountRank rank))
        (Constraint.Constant <| Evaluation.countRank rank hand)
    constraints =
      List.concat
        [ List.map suitConstraint Card.suits
        , List.map rankConstraint Card.ranks
        , structuralConstraints
        ]
  in
    List.foldl Constraint.constrain baseKnowledge constraints


{-| The constraints inherent in the structure of the game.
-}
structuralConstraints : List (Constraint.Constraint Variable)
structuralConstraints =
  totalHighCardPointsInDeck
    :: List.map totalSuitLengthInDeck Card.suits
    ++ List.map totalRankCountInDeck Card.ranks
    ++ List.map totalSuitLengthInHand Seat.relatives
    ++ List.map totalRankCountInHand Seat.relatives
    ++ List.map highCardPointsDefinition Seat.relatives
    ++ List.map lengthPointsDefinition Seat.relatives
    ++ List.map uncommittedPointsDefinition Seat.relatives
    ++ crossMap shortnessPointsDefinition Card.suits Seat.relatives
    ++ crossMap pointsWithTrumpDefinition Card.suits Seat.relatives


{-| There are exactly 40 HCP in the deck.
-}
totalHighCardPointsInDeck : Constraint.Constraint Variable
totalHighCardPointsInDeck =
  Constraint.Equal
    (Constraint.Add <| List.map (\seat -> Constraint.Variable <| Var seat HighCardPoints) Seat.relatives)
    (Constraint.Constant 40)


{-| There are exactly 13 cards in each suit in the deck.
-}
totalSuitLengthInDeck : Card.Suit -> Constraint.Constraint Variable
totalSuitLengthInDeck suit =
  Constraint.Equal
    (Constraint.Add <| List.map (\seat -> Constraint.Variable <| Var seat (Length suit)) Seat.relatives)
    (Constraint.Constant 13)


{-| There are exactly 4 cards of each rank in the deck.
-}
totalRankCountInDeck : Card.Rank -> Constraint.Constraint Variable
totalRankCountInDeck rank =
  Constraint.Equal
    (Constraint.Add <| List.map (\seat -> Constraint.Variable <| Var seat (CountRank rank)) Seat.relatives)
    (Constraint.Constant 4)


{-| The total length of all suits in a single hand is exactly 13.
-}
totalSuitLengthInHand : Seat.Relative -> Constraint.Constraint Variable
totalSuitLengthInHand seat =
  Constraint.Equal
    (Constraint.Add <| List.map (\suit -> Constraint.Variable <| Var seat (Length suit)) Card.suits)
    (Constraint.Constant 13)


{-| The total count of all ranks in a single hand is exactly 13.
-}
totalRankCountInHand : Seat.Relative -> Constraint.Constraint Variable
totalRankCountInHand seat =
  Constraint.Equal
    (Constraint.Add <| List.map (\rank -> Constraint.Variable <| Var seat (CountRank rank)) Card.ranks)
    (Constraint.Constant 13)


{-| The definition of high card points in a hand.
-}
highCardPointsDefinition : Seat.Relative -> Constraint.Constraint Variable
highCardPointsDefinition seat =
  let
    factors =
      [ (4, Card.Ace)
      , (3, Card.King)
      , (2, Card.Queen)
      , (1, Card.Jack)
      ]
    component (factor, rank) =
      Constraint.Multiply [Constraint.Constant 4, Constraint.Variable <| Var seat (CountRank rank)]
  in
    Constraint.Equal
      (Constraint.Add <| List.map component factors)
      (Constraint.Variable <| Var seat HighCardPoints)


{-| The definition of length points in a hand.
-}
lengthPointsDefinition : Seat.Relative -> Constraint.Constraint Variable
lengthPointsDefinition seat =
  let
    component suit =
      Constraint.Max
        [ Constraint.Add
            [ Constraint.Variable <| Var seat (Length suit)
            , Constraint.Constant (-4)
            ]
        , Constraint.Constant 0
        ]
  in
    Constraint.Equal
      (Constraint.Add <| List.map component Card.suits)
      (Constraint.Variable <| Var seat LengthPoints)


{- | The definition of uncommitted points (the points measure used before
a fit, if one exists, has been identified).
-}
uncommittedPointsDefinition : Seat.Relative -> Constraint.Constraint Variable
uncommittedPointsDefinition seat =
  let
    components =
      [ Constraint.Variable <| Var seat HighCardPoints
      , Constraint.Variable <| Var seat LengthPoints
      ]
  in
    Constraint.Equal
      (Constraint.Add components)
      (Constraint.Variable <| Var seat UncommittedPoints)


{-| The definition of shortness points for a given fit.
-}
shortnessPointsDefinition : Card.Suit -> Seat.Relative -> Constraint.Constraint Variable
shortnessPointsDefinition trumpSuit seat =
  let
    countedSuits =
      List.filter ((/=) trumpSuit) Card.suits
    component suit =
      Constraint.Max
        [ Constraint.Add
            [ Constraint.Constant 3
            , Constraint.Negate (Constraint.Variable <| Var seat (Length suit))
            ]
        , Constraint.Constant 0
        ]
  in
    Constraint.Equal
      (Constraint.Add <| List.map component countedSuits)
      (Constraint.Variable <| Var seat (ShortnessPointsWithTrump trumpSuit))


{-| The definition of points when a fit is known.
-}
pointsWithTrumpDefinition : Card.Suit -> Seat.Relative -> Constraint.Constraint Variable
pointsWithTrumpDefinition trumpSuit seat =
  let
    components =
      [ Constraint.Variable <| Var seat HighCardPoints
      , Constraint.Variable <| Var seat (ShortnessPointsWithTrump trumpSuit)
      ]
  in
    Constraint.Equal
      (Constraint.Add components)
      (Constraint.Variable <| Var seat (PointsWithTrump trumpSuit))


{-| Add the hand held by a seat.
-}
addHand : Seat.Relative -> List Card.Card -> Knowledge -> Knowledge
addHand _ _ knowledge = knowledge


{-| Get a piece of information.
-}
get : Seat.Relative -> Metric -> Knowledge -> Constraint.Range
get seat metric knowledge =
  Constraint.possibleValues (Var seat metric) knowledge
