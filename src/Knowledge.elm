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
import Evaluation
import Seat
import Solver

import List.Extra


{-| A metric used to evaluate a player's hand.
-}
type Metric
  = HighCardPoints
  | LengthPoints
  | LengthPointsWithinSuit Card.Suit
  | UncommittedPoints
  | ShortnessPointsWithTrump Card.Suit
  | ShortnessPointsWithinSuit Card.Suit
  | PointsWithTrump Card.Suit
  | Length Card.Suit
  | CountRank Card.Rank
  | PlayingTricks
  | QuickLosers Card.Suit


{-| List of all possible metrics.
-}
allMetrics : List Metric
allMetrics =
  let
    simpleMetrics =
      [ HighCardPoints
      , LengthPoints
      , UncommittedPoints
      , PlayingTricks
      ]
    suitMetricCtors =
      [ LengthPointsWithinSuit
      , ShortnessPointsWithTrump
      , ShortnessPointsWithinSuit
      , PointsWithTrump
      , Length
      , QuickLosers
      ]
    suitMetrics =
      cartesianMap (<|) suitMetricCtors Card.suits
    rankMetricCtors =
      [ CountRank
      ]
    rankMetrics =
      cartesianMap (<|) rankMetricCtors Card.ranks
  in
    simpleMetrics ++ suitMetrics ++ rankMetrics


{-| The variable used in the player's knowledge representation.
-}
type Variable =
  Variable Seat.Relative Metric


{-| List of all possible variables.
-}
allVariables : List Variable
allVariables =
  cartesianMap Variable Seat.relatives allMetrics


{-| Representation of a player's total knowledge.
-}
type alias Knowledge =
  Solver.Problem Variable


{-| The unconstrained variables composing the knowledge state.
-}
baseKnowledge : Knowledge
baseKnowledge =
  let
    constraints =
      lowerBoundConstraints ++ structuralConstraints
  in
    List.foldl Solver.addConstraint Solver.empty constraints


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
      Solver.equal
        (Solver.variable <| Variable Seat.Self (Length suit))
        (Solver.constant <| Evaluation.length suit hand)
    rankConstraint rank =
      Solver.equal
        (Solver.variable <| Variable Seat.Self (CountRank rank))
        (Solver.constant <| Evaluation.countRank rank hand)
    constraints =
      List.concat
        [ List.map suitConstraint Card.suits
        , List.map rankConstraint Card.ranks
        ]
  in
    List.foldl Solver.addConstraint baseKnowledge constraints


{-| Constraints that put a lower bound of 0 on each variable.
-}
lowerBoundConstraints : List (Solver.Constraint Variable)
lowerBoundConstraints =
  let
    lowerBoundOfZero var =
      Solver.variable var `Solver.greaterThanOrEqual` Solver.constant 0
  in
    List.map lowerBoundOfZero allVariables


{-| The constraints inherent in the structure of the game.
-}
structuralConstraints : List (Solver.Constraint Variable)
structuralConstraints =
  totalHighCardPointsInDeck
    :: List.map totalSuitLengthInDeck Card.suits
    ++ List.map totalRankCountInDeck Card.ranks
    ++ List.map totalSuitLengthInHand Seat.relatives
    ++ List.map totalRankCountInHand Seat.relatives
    ++ List.map highCardPointsDefinition Seat.relatives
    ++ crossMap lengthPointsWithinSuit Card.suits Seat.relatives
    ++ List.map lengthPointsDefinition Seat.relatives
    ++ List.map maximumLengthPointsInHand Seat.relatives
    ++ List.map uncommittedPointsDefinition Seat.relatives
    ++ crossMap shortnessPointsWithinSuit Card.suits Seat.relatives
    ++ crossMap shortnessPointsDefinition Card.suits Seat.relatives
    ++ crossMap pointsWithTrumpDefinition Card.suits Seat.relatives


{-| There are exactly 40 HCP in the deck.
-}
totalHighCardPointsInDeck : Solver.Constraint Variable
totalHighCardPointsInDeck =
  Solver.equal
    (Solver.sum <| List.map (\seat -> Solver.variable <| Variable seat HighCardPoints) Seat.relatives)
    (Solver.constant 40)


{-| There are exactly 13 cards in each suit in the deck.
-}
totalSuitLengthInDeck : Card.Suit -> Solver.Constraint Variable
totalSuitLengthInDeck suit =
  Solver.equal
    (Solver.sum <| List.map (\seat -> Solver.variable <| Variable seat (Length suit)) Seat.relatives)
    (Solver.constant 13)


{-| There are exactly 4 cards of each rank in the deck.
-}
totalRankCountInDeck : Card.Rank -> Solver.Constraint Variable
totalRankCountInDeck rank =
  Solver.equal
    (Solver.sum <| List.map (\seat -> Solver.variable <| Variable seat (CountRank rank)) Seat.relatives)
    (Solver.constant 4)


{-| The total length of all suits in a single hand is exactly 13.
-}
totalSuitLengthInHand : Seat.Relative -> Solver.Constraint Variable
totalSuitLengthInHand seat =
  Solver.equal
    (Solver.sum <| List.map (\suit -> Solver.variable <| Variable seat (Length suit)) Card.suits)
    (Solver.constant 13)


{-| The total count of all ranks in a single hand is exactly 13.
-}
totalRankCountInHand : Seat.Relative -> Solver.Constraint Variable
totalRankCountInHand seat =
  Solver.equal
    (Solver.sum <| List.map (\rank -> Solver.variable <| Variable seat (CountRank rank)) Card.ranks)
    (Solver.constant 13)


{-| The definition of high card points in a hand.
-}
highCardPointsDefinition : Seat.Relative -> Solver.Constraint Variable
highCardPointsDefinition seat =
  let
    factors =
      [ (4, Card.Ace)
      , (3, Card.King)
      , (2, Card.Queen)
      , (1, Card.Jack)
      ]
    component (factor, rank) =
      Solver.multiply factor (Solver.variable <| Variable seat (CountRank rank))
  in
    Solver.equal
      (Solver.sum <| List.map component factors)
      (Solver.variable <| Variable seat HighCardPoints)


{-| Calculation of length points within a single suit in a hand.
-}
lengthPointsWithinSuit : Card.Suit -> Seat.Relative -> Solver.Constraint Variable
lengthPointsWithinSuit suit seat =
  Solver.ifThenElse
    (Solver.greaterThan
      (Solver.variable <| Variable seat (Length suit))
      (Solver.constant 4))
    (Solver.equal
      (Solver.variable <| Variable seat (LengthPointsWithinSuit suit))
      (Solver.subtract
        (Solver.variable <| Variable seat (Length suit))
        (Solver.constant 4)))
    (Solver.equal
      (Solver.variable <| Variable seat (LengthPointsWithinSuit suit))
      (Solver.constant 0))


{-| The definition of length points in a hand.
-}
lengthPointsDefinition : Seat.Relative -> Solver.Constraint Variable
lengthPointsDefinition seat =
  Solver.equal
    (Solver.variable <| Variable seat LengthPoints)
    (Solver.sum <| List.map (Solver.variable << Variable seat << LengthPointsWithinSuit) Card.suits)


{-| The maximum number of length points possible in a hand.
-}
maximumLengthPointsInHand : Seat.Relative -> Solver.Constraint Variable
maximumLengthPointsInHand seat =
  Solver.lessThanOrEqual
    (Solver.variable <| Variable seat LengthPoints)
    (Solver.constant 9)


{-| The definition of uncommitted points (the points measure used before
a fit, if one exists, has been identified).
-}
uncommittedPointsDefinition : Seat.Relative -> Solver.Constraint Variable
uncommittedPointsDefinition seat =
  Solver.equal
    (Solver.add
      (Solver.variable <| Variable seat HighCardPoints)
      (Solver.variable <| Variable seat LengthPoints))
    (Solver.variable <| Variable seat UncommittedPoints)


{-| Calculation of the shortness points within a single suit in a hand,
assuming that a fit has been found in a different suit.
-}
shortnessPointsWithinSuit : Card.Suit -> Seat.Relative -> Solver.Constraint Variable
shortnessPointsWithinSuit suit seat =
  Solver.ifThenElse
    (Solver.lessThan
      (Solver.variable <| Variable seat (Length suit))
      (Solver.constant 3))
    (Solver.equal
      (Solver.variable <| Variable seat (ShortnessPointsWithinSuit suit))
      (Solver.subtract
        (Solver.constant 3)
        (Solver.variable <| Variable seat (Length suit))))
    (Solver.equal
      (Solver.variable <| Variable seat (ShortnessPointsWithinSuit suit))
      (Solver.constant 0))


{-| The definition of shortness points for a given fit.
-}
shortnessPointsDefinition : Card.Suit -> Seat.Relative -> Solver.Constraint Variable
shortnessPointsDefinition trumpSuit seat =
  let
    countedSuits =
      List.filter ((/=) trumpSuit) Card.suits
  in
    Solver.equal
      (Solver.sum <| List.map (Solver.variable << Variable seat << ShortnessPointsWithinSuit) countedSuits)
      (Solver.variable <| Variable seat (ShortnessPointsWithTrump trumpSuit))


{-| The definition of points when a fit is known.
-}
pointsWithTrumpDefinition : Card.Suit -> Seat.Relative -> Solver.Constraint Variable
pointsWithTrumpDefinition trumpSuit seat =
  Solver.equal
    (Solver.add
      (Solver.variable <| Variable seat HighCardPoints)
      (Solver.variable <| Variable seat (ShortnessPointsWithTrump trumpSuit)))
    (Solver.variable <| Variable seat (PointsWithTrump trumpSuit))


{-| Add the hand held by a seat.
-}
addHand : Seat.Relative -> List Card.Card -> Knowledge -> Knowledge
addHand _ _ knowledge = knowledge


{-| Get a piece of information.
-}
get : Seat.Relative -> Metric -> Knowledge -> Solver.Range
get seat metric knowledge =
  Solver.possibleValues (Variable seat metric) knowledge


{-| Apply a function to each pair in a cartesian product.
-}
cartesianMap : (a -> b -> c) -> List a -> List b -> List c
cartesianMap f xs ys =
  xs `List.Extra.andThen` \x ->
    ys `List.Extra.andThen` \y ->
      [f x y]
