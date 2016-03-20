module Tests where

import CardTests

import ElmTest exposing (Test, suite)


all : Test
all =
  suite "A Test Suite"
    [ CardTests.all
    ]
