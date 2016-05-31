module Constraint exposing
  ( Range
  , Term (..)
  , Constraint (..)
  , State

  , initialize
  , range
  , possibleValues
  , constrain
  )

{-| Model for bounded integer variables constrainted by relationships
with one another, such that new constraints can be added incrementally.
-}

import DictSet exposing (DictSet)
import EveryDict exposing (EveryDict)
import List.Extra
import Set exposing (Set)


{-| The possible values a variable can have.
-}
type alias Range = Set Int


{-| A value term inside a constraint.
-}
type Term var
  = Constant Int
  | Variable var


{-| An individual constraint over terms.
-}
type Constraint var
  = LessThan (Term var) (Term var)


{-| Unique identifier for a constraint.
-}
type alias ConstraintId = Int


{-| The collection of variable states and the currently known
constraints among them.
-}
type State var =
  State
    { variables : EveryDict var Range
    , constraints : EveryDict var (DictSet String (Constraint var))
    }


{-| Initialie a new state with the given variables.
-}
initialize : List (var, Range) -> State var
initialize variables =
  State
    { variables = EveryDict.fromList variables
    , constraints = EveryDict.empty
    }


{-| Create a simple bounded range.
-}
range : Int -> Int -> Range
range lo hi = Set.fromList [lo .. hi]


{-| Get the possible values for a variable.  Uninitialized variables
have no possible values.
-}
possibleValues : var -> State var -> Range
possibleValues var (State st) =
  Maybe.withDefault Set.empty <| EveryDict.get var st.variables


{-| Get the smallest possible value for a variable.
-}
smallestValue : var -> State var -> Maybe Int
smallestValue var state =
  List.head <| Set.toList <| possibleValues var state


{-| Get the largest possible value for a variable.
-}
largestValue : var -> State var -> Maybe Int
largestValue var state =
  List.Extra.last <| Set.toList <| possibleValues var state


{-| Get the constraints that involve a particular variable.
-}
constraintsWith : var -> State var -> DictSet String (Constraint var)
constraintsWith var (State st) =
  Maybe.withDefault (DictSet.empty toString) <| EveryDict.get var st.constraints


{-| Add a new constraint.
-}
constrain : Constraint var -> State var -> State var
constrain constraint (State st) =
  let
    vars = variables constraint
    worklist = DictSet.singleton toString constraint
  in
    if
      List.all (\var -> EveryDict.member var st.variables) vars
    then
      updateVariables worklist <| registerConstraint constraint vars (State st)
    else
      (State st)


{-| Register a validated constraint with its variables.
-}
registerConstraint : Constraint var -> List var -> State var -> State var
registerConstraint constraint vars (State st) =
  let
    doAdd value =
      case value of
        Just constraints -> Just <| DictSet.insert constraint constraints
        Nothing -> Just <| DictSet.singleton toString constraint
    addConstraint var constraints =
      EveryDict.update var doAdd constraints
  in
    State { st | constraints = List.foldl addConstraint st.constraints vars }


{-| Get the variables present in a constraint, with duplicates removed.
-}
variables : Constraint var -> List var
variables constraint =
  let
    terms =
      case constraint of
        LessThan lhs rhs -> [lhs, rhs]
    getVar term =
      case term of
        Variable var -> Just var
        _ -> Nothing
    vars =
      List.filterMap getVar terms
  in
    DictSet.toList <| DictSet.fromList toString vars


{-| Update the possible values of variables given the worklist of
constraints to (re)evaluate.
-}
updateVariables : DictSet String (Constraint var) -> State var -> State var
updateVariables worklist state =
  case takeWorkItem worklist of
    Nothing ->
      state
    Just (item, rest) ->
      let
        state' =
          enforceConstraint item state
        changes =
          changedVariables state state'
        worklist' =
          List.map (\var -> constraintsWith var state') changes
            |> List.foldl DictSet.union (DictSet.empty toString)
            |> DictSet.remove item
            |> DictSet.union rest
      in
        updateVariables worklist' state'


{-| Enforce a single constraint on the existing state.
-}
enforceConstraint : Constraint var -> State var -> State var
enforceConstraint constraint state =
  case constraint of
    LessThan (Constant bound) (Variable var) ->
      filterRange var (\value -> bound < value) state
    LessThan (Variable var) (Constant bound) ->
      filterRange var (\value -> value < bound) state
    LessThan (Variable left) (Variable right) ->
      case (smallestValue left state, largestValue right state) of
        (Just smallestLeft, Just largestRight) ->
          state
            |> filterRange left (\value -> value < largestRight)
            |> filterRange right (\value -> smallestLeft < value)
        _ ->
          state
    _ ->
      Debug.crash ("not implemented: " ++ toString constraint)


{-| Filter the range of a variable.
-}
filterRange : var -> (Int -> Bool) -> State var -> State var
filterRange var pred (State st) =
  let
    transform value =
      case value of
        Just set -> Just (Set.filter pred set)
        Nothing -> Nothing
  in
    State { st | variables = EveryDict.update var transform st.variables }


{-| Find the variables that have different ranges between two
snapshots of the state.
-}
changedVariables : State var -> State var -> List var
changedVariables (State oldSt) (State newSt) =
  EveryDict.keys oldSt.variables
    |> List.filter (\var -> EveryDict.get var oldSt.variables /= EveryDict.get var newSt.variables)


{-| Take an arbitrary item out of a work list.
-}
takeWorkItem : DictSet comparable a -> Maybe (a, DictSet comparable a)
takeWorkItem worklist =
  case DictSet.toList worklist of
    [] ->
      Nothing
    item :: rest ->
      Just (item, DictSet.remove item worklist)
