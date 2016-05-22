module Producers exposing (elementOf)

{-| General utilities for writing new elm-check producers.
-}

import Array exposing (Array)
import Check.Producer exposing (Producer)


{-| A producer for elements sampled from an array of choices.
-}
elementOf : Array a -> Producer a
elementOf elements =
  let
    maxIndex =
      Array.length elements - 1

    toElement index =
      case Array.get index elements of
        Just element -> element
        Nothing -> Debug.crash "failed to generate an appropriate index into the array!"
  in
    Check.Producer.map toElement (Check.Producer.rangeInt 0 maxIndex)
