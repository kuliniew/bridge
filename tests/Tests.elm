module Tests where

import AuctionTests
import Bidding.StandardAmericanTests
import Bidding.StaymanTests
import BiddingTests
import CardTests
import EvaluationTests
import SeatTests
import VulnerabilityTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ AuctionTests.all
    , Bidding.StandardAmericanTests.all
    , Bidding.StaymanTests.all
    , BiddingTests.all
    , CardTests.all
    , EvaluationTests.all
    , SeatTests.all
    , VulnerabilityTests.all
    ]
