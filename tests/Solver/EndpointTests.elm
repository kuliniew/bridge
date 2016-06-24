module Solver.EndpointTests exposing (all)

import OperationTests
import Solver.Endpoint
import TestUtils

import Check
import Check.Producer
import ElmTest
import Lazy.List
import Random
import Random.Extra
import Shrink


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Endpoint"
    [ lessThanOrEqualSuite
    ]


lessThanOrEqualSuite : ElmTest.Test
lessThanOrEqualSuite =
  ElmTest.suite "lessThanOrEqual"
    [ OperationTests.totalOrder Solver.Endpoint.lessThanOrEqual endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "least element"
        `Check.true`
          (\ep -> Solver.Endpoint.NegativeInfinity `Solver.Endpoint.lessThanOrEqual` ep)
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "greatest element"
        `Check.true`
          (\ep -> ep `Solver.Endpoint.lessThanOrEqual` Solver.Endpoint.PositiveInfinity)
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "consistent with integer <="
        `Check.that`
          (\(val1, val2) -> Solver.Endpoint.Point val1 `Solver.Endpoint.lessThanOrEqual` Solver.Endpoint.Point val2)
        `Check.is`
          uncurry (<=)
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.int)
    ]


endpointProducer : Check.Producer.Producer Solver.Endpoint.Endpoint
endpointProducer =
  { generator =
      Random.Extra.frequency
        [ (0.1, Random.Extra.constant Solver.Endpoint.NegativeInfinity)
        , (0.8, Random.map Solver.Endpoint.Point (Random.int Random.minInt Random.maxInt))
        , (0.1, Random.Extra.constant Solver.Endpoint.PositiveInfinity)
        ]
  , shrinker =
      \endpoint ->
        case endpoint of
          Solver.Endpoint.NegativeInfinity ->
            Lazy.List.empty
          Solver.Endpoint.Point value ->
            Shrink.map Solver.Endpoint.Point (Shrink.int value)
          Solver.Endpoint.PositiveInfinity ->
            Lazy.List.empty
  }
