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
    , fromLowerBoundSuite
    , fromUpperBoundSuite
    , fromEndpointsSuite
    , toEndpointsSuite
    , subsetSuite
    , removeLowerBoundSuite
    , removeUpperBoundSuite
    , addSuite
    , negateSuite
    , subtractSuite
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


fromLowerBoundSuite : ElmTest.Test
fromLowerBoundSuite =
  ElmTest.suite "fromLowerBound"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains its bounding value"
        `Check.true`
          (\bound -> Solver.Interval.member bound (Solver.Interval.fromLowerBound bound))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "contains exactly the values greater than or equal to the bound"
        `Check.that`
          (\(bound, value) -> Solver.Interval.member value (Solver.Interval.fromLowerBound bound))
        `Check.is`
          (\(bound, value) -> value >= bound)
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.int)
    ]


fromUpperBoundSuite : ElmTest.Test
fromUpperBoundSuite =
  ElmTest.suite "fromUpperBound"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains its bounding value"
        `Check.true`
          (\bound -> Solver.Interval.member bound (Solver.Interval.fromUpperBound bound))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "contains exactly the values less than or equal to the bound"
        `Check.that`
          (\(bound, value) -> Solver.Interval.member value (Solver.Interval.fromUpperBound bound))
        `Check.is`
          (\(bound, value) -> value <= bound)
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.int)
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


subsetSuite : ElmTest.Test
subsetSuite =
  ElmTest.suite "subset"
    [ OperationTests.partialOrder Solver.Interval.subset intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "consistent with member"
        `Check.true`
          (\(value, _, superset) -> Solver.Interval.member value superset)
        `Check.for`
          Check.Producer.filter
            (\(value, subset, superset) -> Solver.Interval.member value subset && Solver.Interval.subset subset superset)
            (Check.Producer.tuple3 (Check.Producer.int, intervalProducer, intervalProducer))
    ]


removeLowerBoundSuite : ElmTest.Test
removeLowerBoundSuite =
  removeBoundSuite "removeLowerBound" Solver.Interval.removeLowerBound (<=)


removeUpperBoundSuite : ElmTest.Test
removeUpperBoundSuite =
  removeBoundSuite "removeUpperBound" Solver.Interval.removeUpperBound (>=)


removeBoundSuite : String -> (Interval -> Interval) -> (Int -> Int -> Bool) -> ElmTest.Test
removeBoundSuite name removeFunc compareFunc =
  ElmTest.suite name
    [ OperationTests.unaryIdempotent removeFunc intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "preserves emptiness"
        `Check.that`
          (Solver.Interval.isEmpty << removeFunc)
        `Check.is`
          Solver.Interval.isEmpty
        `Check.for`
          intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "returns a superset"
        `Check.true`
          (\interval -> Solver.Interval.subset interval (removeFunc interval))
        `Check.for`
          intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "contains the expected values"
        `Check.true`
          (\(interval, _, otherValue) -> Solver.Interval.member otherValue (removeFunc interval))
        `Check.for`
          Check.Producer.filter
            (\(interval, value, otherValue) -> Solver.Interval.member value interval && compareFunc otherValue value)
            (Check.Producer.tuple3 (intervalProducer, Check.Producer.int, Check.Producer.int))
    ]


addSuite : ElmTest.Test
addSuite =
  ElmTest.suite "add"
    [ OperationTests.commutativeMonoid Solver.Interval.add (Solver.Interval.singleton 0) intervalProducer

    , OperationTests.leftAnnihilator Solver.Interval.add Solver.Interval.empty intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "contains the sums of values from the input intervals"
        `Check.true`
          (\(interval1, value1, interval2, value2) -> Solver.Interval.member (value1 + value2) (interval1 `Solver.Interval.add` interval2))
        `Check.for`
          Check.Producer.filter
            (\(interval1, value1, interval2, value2) -> Solver.Interval.member value1 interval1 && Solver.Interval.member value2 interval2)
            (Check.Producer.tuple4 (intervalProducer, Check.Producer.int, intervalProducer, Check.Producer.int))
    ]


negateSuite : ElmTest.Test
negateSuite =
  ElmTest.suite "negate"
    [ TestUtils.generativeTest <|
        Check.claim
          "is its own inverse"
        `Check.that`
          (Solver.Interval.negate << Solver.Interval.negate)
        `Check.is`
          identity
        `Check.for`
          intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "contains exactly negations of values from the input interval"
        `Check.that`
          (\(interval, value) -> Solver.Interval.member (negate value) (Solver.Interval.negate interval))
        `Check.is`
          (\(interval, value) -> Solver.Interval.member value interval)
        `Check.for`
          Check.Producer.tuple (intervalProducer, Check.Producer.int)
    ]


subtractSuite : ElmTest.Test
subtractSuite =
  ElmTest.suite "subtract"
    [ OperationTests.leftAnnihilator Solver.Interval.subtract Solver.Interval.empty intervalProducer

    , OperationTests.rightAnnihilator Solver.Interval.subtract Solver.Interval.empty intervalProducer

    , OperationTests.rightIdentity Solver.Interval.subtract (Solver.Interval.singleton 0) intervalProducer

    , TestUtils.generativeTest <|
        Check.claim
          "contains the differences of values from the input intervals"
        `Check.true`
          (\(interval1, value1, interval2, value2) -> Solver.Interval.member (value1 - value2) (interval1 `Solver.Interval.subtract` interval2))
        `Check.for`
          Check.Producer.filter
            (\(interval1, value1, interval2, value2) -> Solver.Interval.member value1 interval1 && Solver.Interval.member value2 interval2)
            (Check.Producer.tuple4 (intervalProducer, Check.Producer.int, intervalProducer, Check.Producer.int))
    ]


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
