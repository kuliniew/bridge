module Tests where

import AuctionTests
import CardTests
import SeatTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ AuctionTests.all
    , CardTests.all
    , SeatTests.all
    ]
