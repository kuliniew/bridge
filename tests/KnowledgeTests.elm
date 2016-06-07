module KnowledgeTests exposing (all)

import Card
import Constraint
import Knowledge
import Seat

import ElmTest
import Set exposing (Set)


all : ElmTest.Test
all =
  ElmTest.suite "Knowledge"
    [ addHandSuite
    ]


addHandSuite : ElmTest.Test
addHandSuite =
  let
    hand =
      { spades = [ Card.Ace, Card.Ten, Card.Nine, Card.Eight ]
      , hearts = [ Card.King ]
      , diamonds = [ Card.Queen, Card.Ten, Card.Nine, Card.Eight, Card.Seven ]
      , clubs = [ Card.Ace, Card.Ten, Card.Nine ]
      }
    knowledge =
      Knowledge.create (Card.fromSuits hand)
  in
    -- TODO: lots more test cases for these
    ElmTest.suite "addHand"
      [ ElmTest.suite "Self"
          [ ElmTest.test "high card points" <|
              assertSetsEqual (Set.singleton 13) (Knowledge.get Seat.Self Knowledge.HighCardPoints knowledge)
          ]
      , ElmTest.suite "Partner"
          [ ElmTest.test "high card points" <|
              assertSetsEqual (Constraint.range 0 (40 - 13)) (Knowledge.get Seat.Partner Knowledge.HighCardPoints knowledge)
          ]
      ]


assertSetsEqual : Set comparable -> Set comparable -> ElmTest.Assertion
assertSetsEqual xs ys = ElmTest.assertEqual (Set.toList xs) (Set.toList ys)
