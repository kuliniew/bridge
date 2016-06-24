module Tests exposing (all)

import AuctionTests
import Bidding.GerberTests
import Bidding.JacobyTransferTests
import Bidding.StandardAmericanTests
import Bidding.StaymanTests
import BiddingTests
import CardTests
import EvaluationTests
import SeatTests
import Solver.EndpointTests
import Solver.IntervalTests
import Solver.ProblemTests
import Solver.RangeTests
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
    , EvaluationTests.all
    , SeatTests.all
    , Solver.EndpointTests.all
    , Solver.IntervalTests.all
    , Solver.ProblemTests.all
    , Solver.RangeTests.all
    , VulnerabilityTests.all
    ]
