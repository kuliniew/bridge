module Tests where

import AuctionTests
import BiddingTests
import CardTests
import SeatTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ AuctionTests.all
    , BiddingTests.all
    , CardTests.all
    , SeatTests.all
    ]
