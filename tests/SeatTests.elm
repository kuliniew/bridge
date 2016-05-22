module SeatTests exposing (all)

import Seat
import Seat.Producer
import TestUtils

import Check
import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "Seat"
    [ nextSuite
    ]


nextSuite : ElmTest.Test
nextSuite =
  ElmTest.suite "next"
    [ TestUtils.generativeTest <|
        Check.claim
          "returns a different player"
        `Check.false`
          (\seat -> Seat.next seat == seat)
        `Check.for`
          Seat.Producer.seat

    , TestUtils.generativeTest <|
        Check.claim
          "cycles every four calls"
        `Check.that`
          (Seat.next << Seat.next << Seat.next << Seat.next)
        `Check.is`
          identity
        `Check.for`
          Seat.Producer.seat
    ]
