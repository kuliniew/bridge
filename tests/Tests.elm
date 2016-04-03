module Tests where

import AuctionTests
import Bidding.StandardAmericanTests
import BiddingTests
import CardTests
import EvaluationTests
import SeatTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ AuctionTests.all
    , Bidding.StandardAmericanTests.all
    , BiddingTests.all
    , CardTests.all
    , EvaluationTests.all
    , SeatTests.all
    ]
