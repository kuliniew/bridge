module Seat (Seat (..), Each, lookup) where


type Seat
  = West
  | North
  | East
  | South


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
