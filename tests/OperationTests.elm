module OperationTests exposing
  ( partialOrder
  , totalOrder
  , boundedLattice
  , commutativeMonoid

  , commutative
  , unaryIdempotent
  , rightIdentity
  , leftAnnihilator
  , rightAnnihilator
  )

{-| Test suites for various mathematical properties.
-}

import TestUtils

import Check
import Check.Producer
import ElmTest


{-| Test that a relation is a partial order.
-}
partialOrder : (a -> a -> Bool) -> Check.Producer.Producer a -> ElmTest.Test
partialOrder operation producer =
  ElmTest.suite "partial order"
    [ antisymmetric operation producer
    , transitive operation producer

    , TestUtils.generativeTest <|
        Check.claim
          "reflexive"
        `Check.true`
          (\a -> a `operation` a)
        `Check.for`
          producer
    ]


{-| Test that a relation is a total order.
-}
totalOrder : (a -> a -> Bool) -> Check.Producer.Producer a -> ElmTest.Test
totalOrder operation producer =
  ElmTest.suite "total order"
    [ antisymmetric operation producer
    , transitive operation producer

    , TestUtils.generativeTest <|
        Check.claim
          "total"
        `Check.true`
          (\(a, b) -> a `operation` b || b `operation` a)
        `Check.for`
          Check.Producer.tuple (producer, producer)
    ]


{-| Test that a pair of operations form a bounded lattice.
-}
boundedLattice : (a -> a -> a) -> a -> (a -> a -> a) -> a -> Check.Producer.Producer a -> ElmTest.Test
boundedLattice join bottom meet top producer =
  ElmTest.suite "bounded lattice"
    [ ElmTest.suite "join"
        [ commutative join producer
        , associative join producer
        , absorption meet join producer
        , binaryIdempotent join producer
        , leftIdentity join bottom producer
        ]
    , ElmTest.suite "meet"
        [ commutative meet producer
        , associative meet producer
        , absorption join meet producer
        , binaryIdempotent meet producer
        , leftIdentity meet top producer
        ]
    ]


{-| Test that an operation is a commutative monoid.
-}
commutativeMonoid : (a -> a -> a) -> a -> Check.Producer.Producer a -> ElmTest.Test
commutativeMonoid operation identityElement producer =
  ElmTest.suite "commutative monoid"
    [ commutative operation producer
    , associative operation producer
    , leftIdentity operation identityElement producer
    ]


{-| Test that a relation is antisymmetric.
-}
antisymmetric : (a -> a -> Bool) -> Check.Producer.Producer a -> ElmTest.Test
antisymmetric operation producer =
  TestUtils.generativeTest <|
    Check.claim
      "antisymmetric"
    `Check.true`
      uncurry (==)
    `Check.for`
      Check.Producer.filter
        (\(a, b) -> a `operation` b && b `operation` a)
        (Check.Producer.tuple (producer, producer))


{-| Test that a relation is transitive.
-}
transitive : (a -> a -> Bool) -> Check.Producer.Producer a -> ElmTest.Test
transitive operation producer =
  TestUtils.generativeTest <|
    Check.claim
      "transitive"
    `Check.true`
      (\(a, _, c) -> a `operation` c)
    `Check.for`
      Check.Producer.filter
        (\(a, b, c) -> a `operation` b && b `operation` c)
        (Check.Producer.tuple3 (producer, producer, producer))


{-| Test that an operation is commutative.
-}
commutative : (a -> a -> b) -> Check.Producer.Producer a -> ElmTest.Test
commutative operation producer =
  TestUtils.generativeTest <|
    Check.claim
      "commutative"
    `Check.that`
      uncurry operation
    `Check.is`
      uncurry (flip operation)
    `Check.for`
      Check.Producer.tuple (producer, producer)


{-| Test that an operation is associative.
-}
associative : (a -> a -> a) -> Check.Producer.Producer a -> ElmTest.Test
associative operation producer =
  TestUtils.generativeTest <|
    Check.claim
      "associative"
    `Check.that`
      (\(x, y, z) -> x `operation` (y `operation` z))
    `Check.is`
      (\(x, y, z) -> (x `operation` y) `operation` z)
    `Check.for`
      Check.Producer.tuple3 (producer, producer, producer)


{-| Test that two operations satisfy the absorption law.
-}
absorption : (a -> a -> a) -> (a -> a -> a) -> Check.Producer.Producer a -> ElmTest.Test
absorption firstOperation secondOperation producer =
  TestUtils.generativeTest <|
    Check.claim
      "absorption"
    `Check.that`
      (\(x, y) -> x `secondOperation` (x `firstOperation` y))
    `Check.is`
      fst
    `Check.for`
      Check.Producer.tuple (producer, producer)


{-| Test that a unary operation is idempotent.
-}
unaryIdempotent : (a -> a) -> Check.Producer.Producer a -> ElmTest.Test
unaryIdempotent operation producer =
  TestUtils.generativeTest <|
    Check.claim
      "idempotent"
    `Check.that`
      (operation << operation)
    `Check.is`
      operation
    `Check.for`
      producer


{-| Test that a binary operation is idempotent.
-}
binaryIdempotent : (a -> a -> a) -> Check.Producer.Producer a -> ElmTest.Test
binaryIdempotent operation producer =
  TestUtils.generativeTest <|
    Check.claim
      "idempotent"
    `Check.that`
      (\x -> x `operation` x)
    `Check.is`
      identity
    `Check.for`
      producer


{-| Test that an operation has a left-identity.
-}
leftIdentity : (a -> a -> a) -> a -> Check.Producer.Producer a -> ElmTest.Test
leftIdentity operation identityElement producer =
  TestUtils.generativeTest <|
    Check.claim
      "left identity"
    `Check.that`
      operation identityElement
    `Check.is`
      identity
    `Check.for`
      producer


{-| Test that an operation has a right-identity.
-}
rightIdentity : (a -> a -> a) -> a -> Check.Producer.Producer a -> ElmTest.Test
rightIdentity operation identityElement producer =
  TestUtils.generativeTest <|
    Check.claim
      "right identity"
    `Check.that`
      flip operation identityElement
    `Check.is`
      identity
    `Check.for`
      producer


{-| Test that an operation has a left-annihilator.
-}
leftAnnihilator : (a -> a -> a) -> a -> Check.Producer.Producer a -> ElmTest.Test
leftAnnihilator operation annihilatorElement producer =
  TestUtils.generativeTest <|
    Check.claim
      "left annihilator"
    `Check.that`
      operation annihilatorElement
    `Check.is`
      always annihilatorElement
    `Check.for`
      producer


{-| Test that an operation has a right-annihilator.
-}
rightAnnihilator : (a -> a -> a) -> a -> Check.Producer.Producer a -> ElmTest.Test
rightAnnihilator operation annihilatorElement producer =
  TestUtils.generativeTest <|
    Check.claim
      "right annihilator"
    `Check.that`
      flip operation annihilatorElement
    `Check.is`
      always annihilatorElement
    `Check.for`
      producer
