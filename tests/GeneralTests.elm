module GeneralTests exposing (totalOrder)

{-| Non-type-specific property-based tests.
-}

import Check
import Check.Producer
import Check.Test
import ElmTest


{-| Test that a comparison is a total order.
-}
totalOrder : (a -> a -> Order) -> Check.Producer.Producer a -> ElmTest.Test
totalOrder comparison producer =
  let
    le card1 card2 = card1 `comparison` card2 /= GT
  in
    ElmTest.suite "is a total order"
      [ Check.Test.evidenceToTest <| Check.quickCheck <|
          Check.claim
            "is antisymmetric"
          `Check.that`
            uncurry comparison
          `Check.is`
            always EQ
          `Check.for`
            Check.Producer.filter
              ( \(a, b) -> (a `le` b) && (b `le` a) )
              ( Check.Producer.tuple (producer, producer) )

      , Check.Test.evidenceToTest <| Check.quickCheck <|
          Check.claim
            "is transitive"
          `Check.true`
            ( \(a, _, c) -> a `le` c )
          `Check.for`
            Check.Producer.filter
              ( \(a, b, c) -> (a `le` b) && (b `le` c) )
              ( Check.Producer.tuple3 (producer, producer, producer) )
      ]
