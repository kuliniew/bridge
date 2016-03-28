module Seat (Seat (..), seats, next, Each, lookup) where

import Array exposing (Array)


type Seat
  = West
  | North
  | East
  | South


{-| Array of all seats.
-}
seats : Array Seat
seats =
  Array.fromList [West, North, East, South]


{-| Return the next seat to act after this one.
-}
next : Seat -> Seat
next seat =
  case seat of
    West -> North
    North -> East
    East -> South
    South -> West


type alias Each a =
  { west : a
  , north : a
  , east : a
  , south : a
  }


lookup : Seat -> Each a -> a
lookup seat each =
  case seat of
    West -> each.west
    North -> each.north
    East -> each.east
    South -> each.south
