module OperationTests exposing
  ( totalOrder
  )

{-| Test suites for various mathematical properties.
-}

import TestUtils

import Check
import Check.Producer
import ElmTest


{-| Test that a relation is a total order.
-}
totalOrder : (a -> a -> Bool) -> Check.Producer.Producer a -> ElmTest.Test
totalOrder operation producer =
  ElmTest.suite "total order"
    [ TestUtils.generativeTest <|
        Check.claim
          "antisymmetric"
        `Check.true`
          uncurry (==)
        `Check.for`
          Check.Producer.filter
            (\(a, b) -> a `operation` b && b `operation` a)
            (Check.Producer.tuple (producer, producer))

    , TestUtils.generativeTest <|
        Check.claim
          "transitive"
        `Check.true`
          (\(a, _, c) -> a `operation` c)
        `Check.for`
          Check.Producer.filter
            (\(a, b, c) -> a `operation` b && b `operation` c)
            (Check.Producer.tuple3 (producer, producer, producer))

    , TestUtils.generativeTest <|
        Check.claim
          "total"
        `Check.true`
          (\(a, b) -> a `operation` b || b `operation` a)
        `Check.for`
          Check.Producer.tuple (producer, producer)
    ]
