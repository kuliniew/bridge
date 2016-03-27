module CardTests (all) where

import Card exposing (Card)
import Card.Producer
import GeneralTests

import Array
import Check
import Check.Producer
import Check.Test
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Card"
    [ Check.Test.evidenceToTest (Check.quickCheck claims)

    , ElmTest.suite "rankDescending" <|
        [ ElmTest.test "Ace of Hearts < Ten of Spades" <|
            ElmTest.assertEqual LT <| toCard Card.Hearts Card.Ace `Card.rankDescending` toCard Card.Spades Card.Ten

        , ElmTest.test "Ten of Spades > Ace of Hearts" <|
            ElmTest.assertEqual GT <| toCard Card.Spades Card.Ten `Card.rankDescending` toCard Card.Hearts Card.Ace

        , Check.Test.evidenceToTest <| Check.quickCheck <|
            Check.claim
              "ignores suit"
            `Check.that`
              (\(rank, suit1, suit2) -> toCard suit1 rank `Card.rankDescending` toCard suit2 rank)
            `Check.is`
              always EQ
            `Check.for`
              Check.Producer.tuple3 (Card.Producer.rank, Card.Producer.suit, Card.Producer.suit)

        , GeneralTests.totalOrder Card.rankDescending Card.Producer.card
        ]

    , ElmTest.test "deck has 52 cards" <|
        ElmTest.assertEqual 52 (Array.length Card.deck)

    ]

claims : Check.Claim
claims =
  Check.suite "claims"
    [ Check.claim
        "deck contains each card"
      `Check.true`
        (\card -> List.member card (Array.toList Card.deck))
      `Check.for`
        Card.Producer.card
    ]


toCard : Card.Suit -> Card.Rank -> Card
toCard suit rank =
  { suit = suit, rank = rank }
