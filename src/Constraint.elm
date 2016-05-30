module Constraint exposing
  ( Range
  , State

  , initialize
  , possibleValues
  )

{-| Model for bounded integer variables constrainted by relationships
with one another, such that new constraints can be added incrementally.
-}

import EveryDict exposing (EveryDict)

import Set exposing (Set)


{-| The possible values a variable can have.
-}
type alias Range = Set Int


{-| The collection of variable states and the currently known
constraints among them.
-}
type State var =
  State
    { variables : EveryDict var Range
    }


{-| Initialie a new state with the given variables.
-}
initialize : List (var, Range) -> State var
initialize variables =
  State
    { variables = EveryDict.fromList variables
    }


{-| Get the possible values for a variable.  Uninitialized variables
have no possible values.
-}
possibleValues : var -> State var -> Range
possibleValues var (State st) =
  Maybe.withDefault Set.empty <| EveryDict.get var st.variables
