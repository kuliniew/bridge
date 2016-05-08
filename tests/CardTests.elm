module CardTests (all) where

import Card exposing (Card)
import Card.Producer
import GeneralTests
import TestUtils

import Array
import Check
import Check.Producer
import Check.Test
import ElmTest
import List.Extra


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

    , dealTests
    ]


dealTests : ElmTest.Test
dealTests =
  ElmTest.suite "deal"
    [ TestUtils.generativeTest <|
        Check.claim
          "four hands are dealt"
        `Check.that`
          List.length
        `Check.is`
          always 4
        `Check.for`
          Card.Producer.deal

    , TestUtils.generativeTest <|
        Check.claim
          "each hand has 13 cards"
        `Check.true`
          (List.all (\hand -> List.length hand == 13))
        `Check.for`
          Card.Producer.deal

    , let
        -- O(n^2), but List.Extra.dropDuplicates only works with comparable
        unique cards =
          List.Extra.selectSplit cards
            |> List.all (\(_, card, rest) -> not <| List.member card rest)
      in
        TestUtils.generativeTest <|
          Check.claim
            "each card is in exactly one hand"
          `Check.true`
            (unique << List.concat)
          `Check.for`
            Card.Producer.deal
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
