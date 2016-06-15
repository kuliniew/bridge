module Tests exposing (all)

import AuctionTests
import Bidding.GerberTests
import Bidding.JacobyTransferTests
import Bidding.StandardAmericanTests
import Bidding.StaymanTests
import BiddingTests
import CardTests
import ConstraintTests
import EvaluationTests
import IntervalTests
import KnowledgeTests
import SeatTests
import VulnerabilityTests

import ElmTest


all : ElmTest.Test
all =
  ElmTest.suite "A Test Suite"
    [ AuctionTests.all
    , Bidding.GerberTests.all
    , Bidding.JacobyTransferTests.all
    , Bidding.StandardAmericanTests.all
    , Bidding.StaymanTests.all
    , BiddingTests.all
    , CardTests.all
    , ConstraintTests.all
    , EvaluationTests.all
    , IntervalTests.all
    , KnowledgeTests.all
    , SeatTests.all
    , VulnerabilityTests.all
    ]
