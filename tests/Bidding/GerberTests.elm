module Bidding.GerberTests exposing (all)

import Auction
import Bidding
import Bidding.Gerber
import Bidding.TestUtils
import Card
import Vulnerability

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Bidding.Gerber"
    [ askForAcesSuite
    ]


askForAcesSuite : ElmTest.Test
askForAcesSuite =
  let
    yes =
      [Auction.Bid 4 (Just Card.Clubs)]
    no =
      []
    simpleSystem =
      { name = "Gerber Aces test (no extra conditions)"
      , suggestions = \ _ _ -> [Bidding.Gerber.askForAces Nothing]
      }
    simpleUnitTests =
      List.map (Bidding.TestUtils.testBid simpleSystem)
        [ { name = "no voids, no two quick losers"
          , expected = yes
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Ace, Card.Two, Card.Three ]
          }

        , { name = "void in spades"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = []
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , clubs = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          }

        , { name = "void in hearts"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = []
          , diamonds = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , clubs = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          }

        , { name = "void in diamonds"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , diamonds = []
          , clubs = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          }

        , { name = "void in clubs"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four, Card.Five ]
          , hearts = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , clubs = []
          }

        , { name = "two quick losers in spades"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Queen, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Ace, Card.Two, Card.Three ]
          }

        , { name = "two quick losers in hearts"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three ]
          , hearts = [ Card.Queen, Card.Two, Card.Three, Card.Four ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Ace, Card.Two, Card.Three ]
          }

        , { name = "two quick losers in diamonds"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three ]
          , hearts = [ Card.Ace, Card.Two, Card.Three ]
          , diamonds = [ Card.Queen, Card.Two, Card.Three, Card.Four ]
          , clubs = [ Card.Ace, Card.Two, Card.Three ]
          }

        , { name = "two quick losers in clubs"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three ]
          , hearts = [ Card.Ace, Card.Two, Card.Three ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Queen, Card.Two, Card.Three, Card.Four ]
          }
        ]
    conditionedSystem =
      { name = "Gerber Aces test (with extra conditions)"
      , suggestions = \ _ _ -> [Bidding.Gerber.askForAces (Just <| Bidding.Minimum Bidding.HighCardPoints (Bidding.Constant 20))]
      }
    conditionedUnitTests =
      List.map (Bidding.TestUtils.testBid conditionedSystem)
        [ { name = "only meets base requirements"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Two, Card.Three ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Ace, Card.Two, Card.Three ]
          }

        , { name = "only meets additional requirements"
          , expected = no
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.King, Card.Two, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.King, Card.Two, Card.Three ]
          , diamonds = [ Card.Ace, Card.King, Card.Two, Card.Three ]
          , clubs = []
          }

        , { name = "meets all requirements"
          , expected = yes
          , favorability = Vulnerability.Equal
          , history = []
          , spades = [ Card.Ace, Card.Queen, Card.Three, Card.Four ]
          , hearts = [ Card.Ace, Card.Queen, Card.Three ]
          , diamonds = [ Card.Ace, Card.Two, Card.Three ]
          , clubs = [ Card.Ace, Card.Two, Card.Three ]
          }
        ]
  in
    ElmTest.suite "askForAces" (simpleUnitTests ++ conditionedUnitTests)
