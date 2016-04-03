module Tests where

import AuctionTests
import BiddingTests
import CardTests
import EvaluationTests
import SeatTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ AuctionTests.all
    , BiddingTests.all
    , CardTests.all
    , EvaluationTests.all
    , SeatTests.all
    ]
