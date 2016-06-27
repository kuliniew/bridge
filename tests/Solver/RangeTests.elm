module Solver.RangeTests exposing
  ( all
  , rangeProducer
  )

import OperationTests
import Solver.Interval
import Solver.IntervalTests exposing (intervalProducer)
import Solver.Range exposing (Range)
import TestUtils

import Check
import Check.Producer
import ElmTest
import List.Extra


all : ElmTest.Test
all =
  ElmTest.suite "Solver.Range"
    [ emptySuite
    , fullSuite
    , singletonSuite
    , subsetSuite
    , fromIntervalsSuite
    , toIntervalsSuite
    , intersectSuite
    , unionSuite
    , latticeSuite
    ]


emptySuite : ElmTest.Test
emptySuite =
  ElmTest.suite "empty"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains no values"
        `Check.false`
          flip Solver.Range.member Solver.Range.empty
        `Check.for`
          Check.Producer.int

    , ElmTest.test "is empty" <|
        ElmTest.assert <| Solver.Range.isEmpty Solver.Range.empty
    ]


fullSuite : ElmTest.Test
fullSuite =
  ElmTest.suite "full"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains all values"
        `Check.true`
          flip Solver.Range.member Solver.Range.full
        `Check.for`
          Check.Producer.int

    , ElmTest.test "is not empty" <|
        ElmTest.assert <| not <| Solver.Range.isEmpty Solver.Range.full
    ]


singletonSuite : ElmTest.Test
singletonSuite =
  ElmTest.suite "singleton"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains its value"
        `Check.true`
          (\value -> Solver.Range.member value (Solver.Range.singleton value))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "contains no other values"
        `Check.false`
          (\(good, bad) -> Solver.Range.member bad (Solver.Range.singleton good))
        `Check.for`
          Check.Producer.filter
            (uncurry (/=))
            (Check.Producer.tuple (Check.Producer.int, Check.Producer.int))
    ]


subsetSuite : ElmTest.Test
subsetSuite =
  ElmTest.suite "subset"
    [ OperationTests.partialOrder Solver.Range.subset rangeProducer

    , TestUtils.generativeTest <|
        Check.claim
          "consistent with member"
        `Check.true`
          (\(value, _, superset) -> Solver.Range.member value superset)
        `Check.for`
          Check.Producer.filter
            (\(value, subset, superset) -> Solver.Range.member value subset && Solver.Range.subset subset superset)
            (Check.Producer.tuple3 (Check.Producer.int, rangeProducer, rangeProducer))
    ]


fromIntervalsSuite : ElmTest.Test
fromIntervalsSuite =
  ElmTest.suite "fromIntervals"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains the same elements as the input intervals"
        `Check.that`
          (\(value, intervals) -> Solver.Range.member value (Solver.Range.fromIntervals intervals))
        `Check.is`
          (\(value, intervals) -> List.any (Solver.Interval.member value) intervals)
        `Check.for`
          Check.Producer.tuple (Check.Producer.int, Check.Producer.list intervalProducer)
    ]


toIntervalsSuite : ElmTest.Test
toIntervalsSuite =
  ElmTest.suite "toIntervals"
    [ TestUtils.generativeTest <|
        Check.claim
          "is reversed by fromIntervals"
        `Check.that`
          (Solver.Range.fromIntervals << Solver.Range.toIntervals)
        `Check.is`
          identity
        `Check.for`
          rangeProducer

    , let
        pairs =
          List.Extra.selectSplit
            >> List.concatMap (\(_, elem, after) -> List.map ((,) elem) after)
        disjoint interval1 interval2 =
          Solver.Interval.isEmpty <| Solver.Interval.intersect interval1 interval2
      in
        TestUtils.generativeTest <|
          Check.claim
            "disjoint"
          `Check.true`
            (List.all (uncurry disjoint) << pairs << Solver.Range.toIntervals)
          `Check.for`
            rangeProducer

    , ElmTest.test "empty" <|
        ElmTest.assertEqual [] (Solver.Range.toIntervals Solver.Range.empty)

    , ElmTest.test "full" <|
        ElmTest.assertEqual [Solver.Interval.unbounded] (Solver.Range.toIntervals Solver.Range.full)

    , TestUtils.generativeTest <|
        Check.claim
          "singleton"
        `Check.that`
          (Solver.Range.toIntervals << Solver.Range.singleton)
        `Check.is`
          (\value -> [Solver.Interval.singleton value])
        `Check.for`
          Check.Producer.int
    ]


intersectSuite : ElmTest.Test
intersectSuite =
  ElmTest.suite "intersect"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains exactly the values in both ranges"
        `Check.that`
          (\(range1, range2, value) -> Solver.Range.member value (Solver.Range.intersect range1 range2))
        `Check.is`
          (\(range1, range2, value) -> Solver.Range.member value range1 && Solver.Range.member value range2)
        `Check.for`
          Check.Producer.tuple3 (rangeProducer, rangeProducer, Check.Producer.int)
    ]


unionSuite : ElmTest.Test
unionSuite =
  ElmTest.suite "union"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains exactly the values in either range"
        `Check.that`
          (\(range1, range2, value) -> Solver.Range.member value (Solver.Range.union range1 range2))
        `Check.is`
          (\(range1, range2, value) -> Solver.Range.member value range1 || Solver.Range.member value range2)
        `Check.for`
          Check.Producer.tuple3 (rangeProducer, rangeProducer, Check.Producer.int)
    ]


latticeSuite : ElmTest.Test
latticeSuite =
  ElmTest.suite "forms a bounded lattice (union is join, intersect is meet)"
    [ OperationTests.boundedLattice Solver.Range.union Solver.Range.empty Solver.Range.intersect Solver.Range.full rangeProducer
    ]


rangeProducer : Check.Producer.Producer Range
rangeProducer =
  Check.Producer.convert Solver.Range.fromIntervals Solver.Range.toIntervals (Check.Producer.list intervalProducer)
