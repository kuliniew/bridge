module CardTests (all) where

import Card
import Card.Producer

import Array
import Check
import Check.Test
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Card"
    [ Check.Test.evidenceToTest (Check.quickCheck claims)

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
