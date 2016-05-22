module Seat.Producer exposing (seat)

{-| This module contains elm-check producers for Seat.
-}

import Producers
import Seat

import Check.Producer exposing (Producer)


{-| A producer for seats.
-}
seat : Producer Seat.Seat
seat =
  Producers.elementOf Seat.seats
