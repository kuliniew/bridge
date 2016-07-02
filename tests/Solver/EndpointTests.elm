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
    , addSuite
    , negateSuite
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


addSuite : ElmTest.Test
addSuite =
  ElmTest.suite "add"
    [ TestUtils.generativeTest <|
        Check.claim
          "commutative (all values)"
        `Check.that`
          (\(ep1, ep2) -> ep1 `Solver.Endpoint.add` ep2)
        `Check.is`
          (\(ep1, ep2) -> ep2 `Solver.Endpoint.add` ep1)
        `Check.for`
          Check.Producer.tuple (endpointProducer, endpointProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "associative (propagating failures)"
        `Check.that`
          (\(ep1, ep2, ep3) ->
            case ep1 `Solver.Endpoint.add` ep2 of
              Just partial -> partial `Solver.Endpoint.add` ep3
              Nothing -> Nothing)
        `Check.is`
          (\(ep1, ep2, ep3) ->
            case ep2 `Solver.Endpoint.add` ep3 of
              Just partial -> ep1 `Solver.Endpoint.add` partial
              Nothing -> Nothing)
        `Check.for`
          Check.Producer.tuple3 (endpointProducer, endpointProducer, endpointProducer)

    , TestUtils.generativeTest <|
        Check.claim
          "left identity (all values)"
        `Check.that`
          Solver.Endpoint.add (Solver.Endpoint.Point 0)
        `Check.is`
          Just
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "inverse (non-infinite only)"
        `Check.that`
          (\value -> Solver.Endpoint.Point value `Solver.Endpoint.add` (Solver.Endpoint.negate <| Solver.Endpoint.Point value))
        `Check.is`
          (always <| Just <| Solver.Endpoint.Point 0)
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "behaves like integer addition for non-infinite points"
        `Check.that`
          (\(value1, value2) -> Solver.Endpoint.Point value1 `Solver.Endpoint.add` Solver.Endpoint.Point value2)
        `Check.is`
          (\(value1, value2) -> Just <| Solver.Endpoint.Point (value1 + value2))
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "PositiveInfinity dominates integers"
        `Check.that`
          (\value -> Solver.Endpoint.PositiveInfinity `Solver.Endpoint.add` Solver.Endpoint.Point value)
        `Check.is`
          always (Just Solver.Endpoint.PositiveInfinity)
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "NegativeInfinity dominates integers"
        `Check.that`
          (\value -> Solver.Endpoint.NegativeInfinity `Solver.Endpoint.add` Solver.Endpoint.Point value)
        `Check.is`
          always (Just Solver.Endpoint.NegativeInfinity)
        `Check.for`
          Check.Producer.int

    , ElmTest.test "PositiveInfinity + PositiveInfinity" <|
        ElmTest.assertEqual
          (Just Solver.Endpoint.PositiveInfinity)
          (Solver.Endpoint.PositiveInfinity `Solver.Endpoint.add` Solver.Endpoint.PositiveInfinity)

    , ElmTest.test "NegativeInfinity + NegativeInfinity" <|
        ElmTest.assertEqual
          (Just Solver.Endpoint.NegativeInfinity)
          (Solver.Endpoint.NegativeInfinity `Solver.Endpoint.add` Solver.Endpoint.NegativeInfinity)

    , ElmTest.test "PositiveInfinity + NegativeInfinity" <|
        ElmTest.assertEqual
          Nothing
          (Solver.Endpoint.PositiveInfinity `Solver.Endpoint.add` Solver.Endpoint.NegativeInfinity)
    ]


negateSuite : ElmTest.Test
negateSuite =
  ElmTest.suite "negate"
    [ TestUtils.generativeTest <|
        Check.claim
          "behaves like integer negation for non-infinite points"
        `Check.that`
          (Solver.Endpoint.negate << Solver.Endpoint.Point)
        `Check.is`
          (Solver.Endpoint.Point << negate)
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "is its own inverse"
        `Check.that`
          (Solver.Endpoint.negate << Solver.Endpoint.negate)
        `Check.is`
          identity
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "only has 0 as a fixed point"
        `Check.that`
          (\ep -> Solver.Endpoint.negate ep == ep)
        `Check.is`
          (\ep -> ep == Solver.Endpoint.Point 0)
        `Check.for`
          endpointProducer
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
