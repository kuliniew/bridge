module Solver.TestUtils exposing
  ( smallishIntProducer
  )

import Check.Producer


{-| Produce an int small enough to be unlikely to run afowl of
Elm's/Javascript's precision issues beyong 2^31 in tests.
-}
smallishIntProducer : Check.Producer.Producer Int
smallishIntProducer =
  Check.Producer.rangeInt (-100000) 100000
