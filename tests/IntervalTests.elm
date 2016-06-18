module IntervalTests exposing (all)

import Interval exposing (Interval)
import TestUtils

import Check
import Check.Producer
import ElmTest
import Set exposing (Set)


all : ElmTest.Test
all =
  ElmTest.suite "Interval"
    [ emptySuite
    , singletonSuite
    , fromSetSuite
    , toSetSuite
    , addSuite
    , subtractSuite
    , multiplySuite
    , divideSuite
    , minSuite
    , maxSuite
    , negateSuite
    , intersectSuite
    , unionSuite
    ]


emptySuite : ElmTest.Test
emptySuite =
  ElmTest.suite "empty"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains no values"
        `Check.false`
          flip Interval.member Interval.empty
        `Check.for`
          Check.Producer.int
    ]


singletonSuite : ElmTest.Test
singletonSuite =
  ElmTest.suite "singleton"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains its element"
        `Check.true`
          (\val -> Interval.member val (Interval.singleton val))
        `Check.for`
          Check.Producer.int

    , TestUtils.generativeTest <|
        Check.claim
          "contains no other elements"
        `Check.false`
          (\(good, bad) -> Interval.member bad (Interval.singleton good))
        `Check.for`
          Check.Producer.filter
            (\(good, bad) -> good /= bad)
            (Check.Producer.tuple (Check.Producer.int, Check.Producer.int))
    ]


fromSetSuite : ElmTest.Test
fromSetSuite =
  ElmTest.suite "fromSet"
    [ TestUtils.generativeTest <|
        Check.claim
          "contains all of the set's elements"
        `Check.true`
          (\set -> List.all (flip Interval.member <| Interval.fromSet set) (Set.toList set))
        `Check.for`
          setProducer Check.Producer.int
    ]


toSetSuite : ElmTest.Test
toSetSuite =
  ElmTest.suite "toSet"
    [ TestUtils.generativeTest <|
        Check.claim
          "has equivalent membership"
        `Check.that`
          (\(interval, val) -> Interval.member val interval)
        `Check.is`
          (\(interval, val) -> Set.member val (Interval.toSet interval))
        `Check.for`
          Check.Producer.tuple (Interval.producer, Check.Producer.int)

    , TestUtils.generativeTest <|
        Check.claim
          "round trips via fromSet"
        `Check.that`
          (Interval.fromSet << Interval.toSet)
        `Check.is`
          identity
        `Check.for`
          Interval.producer
    ]


addSuite : ElmTest.Test
addSuite =
  binaryOperationSuite "add" Interval.add (+) []


subtractSuite : ElmTest.Test
subtractSuite =
  binaryOperationSuite "subtract" Interval.subtract (-) []


multiplySuite : ElmTest.Test
multiplySuite =
  let
    knownAnswers =
      [ (Interval.singleton 1, Interval.singleton 1, Interval.singleton 1)
      , (Interval.singleton 1, Interval.singleton (-1), Interval.singleton (-1))
      , (Interval.singleton (-1), Interval.singleton 1, Interval.singleton (-1))
      , (Interval.singleton (-1), Interval.singleton (-1), Interval.singleton 1)
      ]
  in
    binaryOperationSuite "multiply" Interval.multiply (*) knownAnswers


divideSuite : ElmTest.Test
divideSuite =
  let
    knownAnswers =
      [ (Interval.singleton 1, Interval.singleton 1, Interval.singleton 1)
      , (Interval.singleton 1, Interval.singleton (-1), Interval.singleton (-1))
      , (Interval.singleton (-1), Interval.singleton 1, Interval.singleton (-1))
      , (Interval.singleton (-1), Interval.singleton (-1), Interval.singleton 1)
      ]
  in
    binaryOperationSuite "divide" Interval.divide (//) knownAnswers


minSuite : ElmTest.Test
minSuite =
  binaryOperationSuite "min" Interval.min min []


maxSuite : ElmTest.Test
maxSuite =
  binaryOperationSuite "max" Interval.max max []


negateSuite : ElmTest.Test
negateSuite =
  ElmTest.suite "negate"
    [ ElmTest.test "empty interval" <|
        ElmTest.assertEqual Interval.empty (Interval.negate Interval.empty)

    , TestUtils.generativeTest <|
        Check.claim
          "is its own inverse"
        `Check.that`
          (Interval.negate >> Interval.negate)
        `Check.is`
          identity
        `Check.for`
          Interval.producer

    , TestUtils.generativeTest <|
        Check.claim
          "contains negations of its elements"
        `Check.true`
          (\(interval, val) -> Interval.member (negate val) (Interval.negate interval))
        `Check.for`
          Interval.producerWithElement
    ]


intersectSuite : ElmTest.Test
intersectSuite =
  let
    memberTest =
      TestUtils.generativeTest <|
        Check.claim
          "only contains values in both intervals"
        `Check.that`
          (\(interval1, interval2, val) -> Interval.member val (Interval.intersect interval1 interval2))
        `Check.is`
          (\(interval1, interval2, val) -> Interval.member val interval1 && Interval.member val interval2)
        `Check.for`
          Check.Producer.tuple3 (Interval.producer, Interval.producer, Check.Producer.int)
  in
    ElmTest.suite "intersect" (memberTest :: emptyBinaryOperationTests Interval.intersect)


unionSuite : ElmTest.Test
unionSuite =
  let
    memberTest =
      TestUtils.generativeTest <|
        Check.claim
          "contains values in either interval (and possibly others)"
        `Check.true`
          (\(interval1, interval2, val) -> Interval.member val (Interval.union interval1 interval2))
        `Check.for`
          Check.Producer.filter
            (\(interval1, interval2, val) -> Interval.member val interval1 || Interval.member val interval2)
            (Check.Producer.tuple3 (Interval.producer, Interval.producer, Check.Producer.int))
  in
    ElmTest.suite "union" [memberTest]


binaryOperationSuite : String -> (Interval -> Interval -> Interval) -> (Int -> Int -> Int) -> List (Interval, Interval, Interval) -> ElmTest.Test
binaryOperationSuite name intervalOp valueOp knownAnswers =
  let
    valueTest =
      TestUtils.generativeTest <|
        Check.claim
          ("applies " ++ name ++ " to pairs of elements")
        `Check.true`
          (\((interval1, val1), (interval2, val2)) -> Interval.member (valueOp val1 val2) (intervalOp interval1 interval2))
        `Check.for`
          Check.Producer.tuple (Interval.producerWithElement, Interval.producerWithElement)
    knownAnswerTest (arg1, arg2, expected) =
      ElmTest.test (name ++ " " ++ toString arg1 ++ " " ++ toString arg2) <|
        ElmTest.assertEqual expected (intervalOp arg1 arg2)
    knownAnswerTests =
      List.map knownAnswerTest knownAnswers
  in
    ElmTest.suite name (valueTest :: emptyBinaryOperationTests intervalOp ++ knownAnswerTests)


emptyBinaryOperationTests : (Interval -> Interval -> Interval) -> List ElmTest.Test
emptyBinaryOperationTests intervalOp =
  [ TestUtils.generativeTest <|
      Check.claim
        "is empty if first argument is empty"
      `Check.that`
        intervalOp Interval.empty
      `Check.is`
        always Interval.empty
      `Check.for`
        Interval.producer

  , TestUtils.generativeTest <|
      Check.claim
        "is empty if second argument is empty"
      `Check.that`
        flip intervalOp Interval.empty
      `Check.is`
        always Interval.empty
      `Check.for`
        Interval.producer
  ]


setProducer : Check.Producer.Producer comparable -> Check.Producer.Producer (Set comparable)
setProducer elemProducer =
  Check.Producer.convert Set.fromList Set.toList (Check.Producer.list elemProducer)
