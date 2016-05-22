module Tests exposing (all)

import AuctionTests
import Bidding.StandardAmericanTests
import Bidding.StaymanTests
import BiddingTests
import CardTests
import EvaluationTests
import SeatTests
import VulnerabilityTests

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "A Test Suite"
    [ AuctionTests.all
    , Bidding.StandardAmericanTests.all
    , Bidding.StaymanTests.all
    , BiddingTests.all
    , CardTests.all
    , EvaluationTests.all
    , SeatTests.all
    , VulnerabilityTests.all
    ]
