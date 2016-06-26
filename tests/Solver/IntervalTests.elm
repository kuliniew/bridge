module Solver.IntervalTests exposing
  ( all
  , intervalProducer
  )

import OperationTests
import Solver.Endpoint
import Solver.EndpointTests exposing (endpointProducer)
import Solver.Interval exposing (Interval)
import TestUtils

import Check
import Check.Producer
import ElmTest
import Lazy.List
import Maybe.Extra
import Random
import Random.Extra
import Shrink


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Interval"
    [ emptySuite
    , unboundedSuite
    , singletonSuite
    , fromEndpointsSuite
    , toEndpointsSuite
    , intersectSuite
    , hullSuite
    , unionSuite
    , latticeSuite
    ]


emptySuite : ElmTest.Test
emptySuite =
  ElmTest.suite "empty"
    [ ElmTest.test "is empty" <|
        ElmTest.assert <| Solver.Interval.isEmpty Solver.Interval.empty

    , TestUtils.generativeTest <|
        Check.claim
          "contains no values"
        `Check.false`
          flip Solver.Interval.member Solver.Interval.empty
        `Check.for`
          Check.Producer.int
    ]


unboundedSuite : ElmTest.Test
unboundedSuite =
  ElmTest.suite "unbounded"
    [ ElmTest.test "is not empty" <|
        ElmTest.assert <| not <| Solver.Interval.isEmpty Solver.Interval.unbounded

    , TestUtils.generativeTest <|
        Check.claim
          "contains all values"
        `Check.true`
          flip Solver.Interval.member Solver.Interval.unbounded
        `Check.for`
          Check.Producer.int
    ]


singletonSuite : ElmTest.Test
singletonSuite =
  ElmTest.suite "singleton"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains its value"
        `Check.true`
          (\value -> Solver.Interval.member value (Solver.Interval.singleton value))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "contains no other values"
        `Check.false`
          (\(good, bad) -> Solver.Interval.member bad (Solver.Interval.singleton good))
        `Check.for`
          Check.Producer.filter
            (uncurry (/=))
            (Check.Producer.tuple (Check.Producer.int, Check.Producer.int))
    ]


fromEndpointsSuite : ElmTest.Test
fromEndpointsSuite =
  ElmTest.suite "fromEndpoints"
    [ TestUtils.generativeTest <|
        Check.claim
          "beginning at PositiveInfinity"
        `Check.that`
          (\hi -> Solver.Interval.fromEndpoints Solver.Endpoint.PositiveInfinity hi)
        `Check.is`
          always Nothing
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "ending at NegativeInfinity"
        `Check.that`
          (\lo -> Solver.Interval.fromEndpoints lo Solver.Endpoint.NegativeInfinity)
        `Check.is`
          always Nothing
        `Check.for`
          endpointProducer

    , TestUtils.generativeTest <|
        Check.claim
          "lo > hi"
        `Check.that`
          uncurry Solver.Interval.fromEndpoints
        `Check.is`
          always Nothing
        `Check.for`
          Check.Producer.filter
            (\(lo, hi) -> lo `Solver.Endpoint.greaterThan` hi)
            (Check.Producer.tuple (endpointProducer, endpointProducer))

    , TestUtils.generativeTest <|
        Check.claim
          "lo <= hi"
        `Check.false`
          (\(lo, hi) -> Maybe.withDefault True <| Maybe.map Solver.Interval.isEmpty <| Solver.Interval.fromEndpoints lo hi)
        `Check.for`
          Check.Producer.filter
            (\(lo, hi) ->
              lo `Solver.Endpoint.lessThanOrEqual` hi &&
              lo /= Solver.Endpoint.PositiveInfinity &&
              hi /= Solver.Endpoint.NegativeInfinity)
            (Check.Producer.tuple (endpointProducer, endpointProducer))
    ]


toEndpointsSuite : ElmTest.Test
toEndpointsSuite =
  ElmTest.suite "toEndpoints"
    [ TestUtils.generativeTest <|
        Check.claim
          "fails only on the empty interval"
        `Check.that`
          (Maybe.Extra.isNothing << Solver.Interval.toEndpoints)
        `Check.is`
          Solver.Interval.isEmpty
        `Check.for`
          intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "is accepted by fromEndpoints"
        `Check.that`
          (Maybe.map (uncurry Solver.Interval.fromEndpoints) << Solver.Interval.toEndpoints)
        `Check.is`
          (Just << Just)
        `Check.for`
          Check.Producer.filter
            (not << Solver.Interval.isEmpty)
            intervalProducer
    ]


intervalProducer : Check.Producer.Producer Interval
intervalProducer =
  let
    nonEmptyGenerator =
      endpointProducer.generator `Random.andThen` \lo ->
        endpointProducer.generator `Random.andThen` \hi ->
          case Solver.Interval.fromEndpoints lo hi of
            Just interval -> Random.Extra.constant interval
            Nothing -> nonEmptyGenerator
    generator =
      Random.Extra.frequency
        [ (0.1, Random.Extra.constant Solver.Interval.empty)
        , (0.9, nonEmptyGenerator)
        ]
    shrinker interval =
      case Solver.Interval.toEndpoints interval of
        Just endpoints ->
          endpoints
            |> Shrink.tuple (endpointProducer.shrinker, endpointProducer.shrinker)
            |> Lazy.List.map (uncurry Solver.Interval.fromEndpoints)
            |> Lazy.List.map (Maybe.withDefault Solver.Interval.empty)
        Nothing ->
          Lazy.List.empty
  in
    { generator = generator
    , shrinker = shrinker
    }


intersectSuite : ElmTest.Test
intersectSuite =
  ElmTest.suite "intersect"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains exactly the values in both intervals"
        `Check.that`
          (\(interval1, interval2, value) -> Solver.Interval.member value (Solver.Interval.intersect interval1 interval2))
        `Check.is`
          (\(interval1, interval2, value) -> Solver.Interval.member value interval1 && Solver.Interval.member value interval2)
        `Check.for`
          Check.Producer.tuple3 (intervalProducer, intervalProducer, Check.Producer.int)
    ]


hullSuite : ElmTest.Test
hullSuite =
  ElmTest.suite "hull"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains the elements in either interval (but maybe others too)"
        `Check.true`
          (\(interval1, interval2, value) -> Solver.Interval.member value (Solver.Interval.hull interval1 interval2))
        `Check.for`
          Check.Producer.filter
            (\(interval1, interval2, value) -> Solver.Interval.member value interval1 || Solver.Interval.member value interval2)
            (Check.Producer.tuple3 (intervalProducer, intervalProducer, Check.Producer.int))
    ]


unionSuite : ElmTest.Test
unionSuite =
  ElmTest.suite "union"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains exactly the values in either interval"
        `Check.that`
          (\(interval1, interval2, value) ->
            case Solver.Interval.union interval1 interval2 of
              Just interval -> Solver.Interval.member value interval
              Nothing -> Debug.crash "should have filtered out non-unionable test cases"
          )
        `Check.is`
          (\(interval1, interval2, value) -> Solver.Interval.member value interval1 || Solver.Interval.member value interval2)
        `Check.for`
          Check.Producer.filter
            (\(interval1, interval2, _) -> Maybe.Extra.isJust <| Solver.Interval.union interval1 interval2)
            (Check.Producer.tuple3 (intervalProducer, intervalProducer, Check.Producer.int))

    , OperationTests.commutative Solver.Interval.union intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "empty interval is identity element"
        `Check.that`
          Solver.Interval.union Solver.Interval.empty
        `Check.is`
          Just
        `Check.for`
          intervalProducer

    , ElmTest.test "overlapping intervals" <|
        ElmTest.assertEqual
          (Just <| Solver.Interval.fromEndpoints (Solver.Endpoint.Point 1) (Solver.Endpoint.Point 10))
          (Maybe.map2 Solver.Interval.union
            (Solver.Interval.fromEndpoints (Solver.Endpoint.Point 1) (Solver.Endpoint.Point 7))
            (Solver.Interval.fromEndpoints (Solver.Endpoint.Point 3) (Solver.Endpoint.Point 10)))

    , ElmTest.test "adjacent intervals" <|
        ElmTest.assertEqual
          (Just <| Solver.Interval.fromEndpoints (Solver.Endpoint.Point 1) (Solver.Endpoint.Point 10))
          (Maybe.map2 Solver.Interval.union
            (Solver.Interval.fromEndpoints (Solver.Endpoint.Point 1) (Solver.Endpoint.Point 5))
            (Solver.Interval.fromEndpoints (Solver.Endpoint.Point 6) (Solver.Endpoint.Point 10)))
    ]


latticeSuite : ElmTest.Test
latticeSuite =
  ElmTest.suite "forms a bounded lattice (hull is join, intersect is meet)"
    [ OperationTests.boundedLattice Solver.Interval.hull Solver.Interval.empty Solver.Interval.intersect Solver.Interval.unbounded intervalProducer
    ]
