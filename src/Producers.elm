module Producers exposing (elementOf)

{-| General utilities for writing new elm-check producers.
-}

import Check.Producer exposing (Producer)
import Random
import Random.Extra
import Shrink


{-| A producer for elements sampled from a list of choices.
-}
elementOf : List a -> Producer a
elementOf elements =
  let
    unwrap result =
      case result of
        Just val -> val
        Nothing -> Debug.crash "gave Producers.elementOf an empty list!"
  in
    { generator =
        Random.Extra.sample elements |> Random.map unwrap
    , shrinker =
        Shrink.noShrink
    }
