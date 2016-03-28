module Tests where

import CardTests
import SeatTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ CardTests.all
    , SeatTests.all
    ]
