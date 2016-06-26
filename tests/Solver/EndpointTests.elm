module Solver.EndpointTests exposing
  ( all
  , endpointProducer
  )

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
    , lessThanSuite
    , greaterThanOrEqualSuite
    , greaterThanSuite
    , compareSuite
    , minSuite
    , maxSuite
    , adjacentSuite
    , latticeSuite
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


lessThanSuite : ElmTest.Test
lessThanSuite =
  ElmTest.suite "lessThan"
    [ TestUtils.generativeTest <|
        Check.claim
          "is consistent with lessThanOrEqual when values are not equal"
        `Check.that`
          uncurry Solver.Endpoint.lessThan
        `Check.is`
          uncurry Solver.Endpoint.lessThanOrEqual
        `Check.for`
          Check.Producer.filter
            (uncurry (/=))
            (Check.Producer.tuple (endpointProducer, endpointProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "always fails for equal values"
        `Check.false`
          (\ep -> ep `Solver.Endpoint.lessThan` ep)
        `Check.for`
          endpointProducer
    ]


greaterThanOrEqualSuite : ElmTest.Test
greaterThanOrEqualSuite =
  ElmTest.suite "greaterThanOrEqual"
    [ TestUtils.generativeTest <|
        Check.claim
          "consistent with lessThanOrEqual"
        `Check.that`
          uncurry Solver.Endpoint.greaterThanOrEqual
        `Check.is`
          (uncurry <| flip Solver.Endpoint.lessThanOrEqual)
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)
    ]


greaterThanSuite : ElmTest.Test
greaterThanSuite =
  ElmTest.suite "greaterThan"
    [ TestUtils.generativeTest <|
        Check.claim
          "consistent with lessThan"
        `Check.that`
          uncurry Solver.Endpoint.greaterThan
        `Check.is`
          (uncurry <| flip Solver.Endpoint.lessThan)
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)
    ]


compareSuite : ElmTest.Test
compareSuite =
  ElmTest.suite "compare"
    [ TestUtils.generativeTest <|
        Check.claim
          "consistent with lessThan"
        `Check.that`
          (\(ep1, ep2) -> (ep1 `Solver.Endpoint.compare` ep2) == LT)
        `Check.is`
          uncurry Solver.Endpoint.lessThan
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "consistent with =="
        `Check.that`
          (\(ep1, ep2) -> (ep1 `Solver.Endpoint.compare` ep2) == EQ)
        `Check.is`
          uncurry (==)
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "consistent with greaterThan"
        `Check.that`
          (\(ep1, ep2) -> (ep1 `Solver.Endpoint.compare` ep2) == GT)
        `Check.is`
          uncurry Solver.Endpoint.greaterThan
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)
    ]


minSuite : ElmTest.Test
minSuite =
  ElmTest.suite "min"
    [ TestUtils.generativeTest <|
        Check.claim
          "returns one of its inputs"
        `Check.true`
          (\(ep1, ep2) ->
            let
              result = Solver.Endpoint.min ep1 ep2
            in
              result == ep1 || result == ep2
          )
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "returns a value <= its inputs"
        `Check.true`
          (\(ep1, ep2) ->
            let
              result = Solver.Endpoint.min ep1 ep2
            in
              result `Solver.Endpoint.lessThanOrEqual` ep1 && result `Solver.Endpoint.lessThanOrEqual` ep2
          )
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)
    ]


maxSuite : ElmTest.Test
maxSuite =
  ElmTest.suite "max"
    [ TestUtils.generativeTest <|
        Check.claim
          "returns one of its inputs"
        `Check.true`
          (\(ep1, ep2) ->
            let
              result = Solver.Endpoint.max ep1 ep2
            in
              result == ep1 || result == ep2
          )
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "returns a value >= its inputs"
        `Check.true`
          (\(ep1, ep2) ->
            let
              result = Solver.Endpoint.max ep1 ep2
            in
              result `Solver.Endpoint.greaterThanOrEqual` ep1 && result `Solver.Endpoint.greaterThanOrEqual` ep2
          )
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)
    ]


adjacentSuite : ElmTest.Test
adjacentSuite =
  ElmTest.suite "adjacent"
    [ TestUtils.generativeTest <|
        Check.claim
          "NegativeInfinity is adjacent to nothing"
        `Check.false`
          Solver.Endpoint.adjacent Solver.Endpoint.NegativeInfinity
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "nothing is adjacent to NegativeInfinity"
        `Check.false`
          flip Solver.Endpoint.adjacent Solver.Endpoint.NegativeInfinity
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "PositiveInfinity is adjacent to nothing"
        `Check.false`
          Solver.Endpoint.adjacent Solver.Endpoint.PositiveInfinity
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "nothing is adjacent to PositiveInfinity"
        `Check.false`
          flip Solver.Endpoint.adjacent Solver.Endpoint.PositiveInfinity
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "points exactly one apart"
        `Check.true`
          (\value -> Solver.Endpoint.adjacent (Solver.Endpoint.Point value) (Solver.Endpoint.Point <| value + 1))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "points exactly one apart, but the wrong way around"
        `Check.false`
          (\value -> Solver.Endpoint.adjacent (Solver.Endpoint.Point value) (Solver.Endpoint.Point <| value - 1))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "points any other distance apart"
        `Check.false`
          (\(value, delta) -> Solver.Endpoint.adjacent (Solver.Endpoint.Point value) (Solver.Endpoint.Point <| value + delta))
        `Check.for`
          Check.Producer.filter
            (\(_, delta) -> delta /= 1)
            (Check.Producer.tuple (Check.Producer.int, Check.Producer.int))
    ]


latticeSuite : ElmTest.Test
latticeSuite =
  ElmTest.suite "forms a bounded lattice (max is join, min is meet)"
    [ OperationTests.boundedLattice Solver.Endpoint.max Solver.Endpoint.NegativeInfinity Solver.Endpoint.min Solver.Endpoint.PositiveInfinity endpointProducer
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
