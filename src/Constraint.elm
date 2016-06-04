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
  | Add (List (Term var))


{-| An individual constraint over terms.
-}
type Constraint var
  = LessThan (Term var) (Term var)
  | Equal (Term var) (Term var)
  | Permutation (List (Term var)) (List (Term var))
  | Or (List (Constraint var))


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
variables =
  let
    termsInConstraint constraint =
      case constraint of
        LessThan left right -> [left, right]
        Equal left right -> [left, right]
        Permutation left right -> left ++ right
        Or subcons -> List.concatMap termsInConstraint subcons
    variablesInTerm term =
      case term of
        Constant _ -> []
        Variable var -> [var]
        Add terms -> List.concatMap variablesInTerm terms
  in
    List.concatMap variablesInTerm << termsInConstraint


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
enforceConstraint constraint (State st) =
  let
    vars =
      variables constraint
    updatedVariables =
      st.variables
        |> EveryDict.filter (\var _ -> List.member var vars)
        |> cartesianProduct
        |> List.filter (flip evaluateConstraint constraint)
        |> unCartesianProduct vars
        |> flip EveryDict.union st.variables
  in
    State { st | variables = updatedVariables }


{-| Convert a map of possible variable values to a list of every
possible variable assignment using those values.
-}
cartesianProduct : EveryDict var Range -> List (EveryDict var Int)
cartesianProduct vars =
  let
    expand pairs =
      case pairs of
        [] ->
          []
        (var, values) :: rest ->
          let
            assignments = List.map (\val -> (var, val)) (Set.toList values)
          in
            case rest of
              [] ->
                List.map (\assn -> [assn]) assignments
              _ ->
                List.concatMap (\others -> List.map (\assn -> assn :: others) assignments) (expand rest)
  in
    EveryDict.toList vars
      |> expand
      |> List.map EveryDict.fromList


{-| Convert a list of possible variable assignments to a map
of possible values for each variable.
-}
unCartesianProduct : List var -> List (EveryDict var Int) -> EveryDict var Range
unCartesianProduct vars assignments =
  let
    insert val current =
      case current of
        Just vals -> Just <| Set.insert val vals
        Nothing -> Just <| Set.singleton val
    joinSingle var val mapping =
      EveryDict.update var (insert val) mapping
    join assignment mapping =
      EveryDict.foldl joinSingle mapping assignment
    emptyMapping =
      EveryDict.fromList <| List.map (\var -> (var, Set.empty)) vars
  in
    List.foldl join emptyMapping assignments


{-| Evaluate a constraint under a given variable assignment.
-}
evaluateConstraint : EveryDict var Int -> Constraint var -> Bool
evaluateConstraint env constraint =
  case constraint of
    LessThan left right ->
      evaluateTerm env left < evaluateTerm env right
    Equal left right ->
      evaluateTerm env left == evaluateTerm env right
    Permutation left right ->
      List.map (evaluateTerm env) left `List.Extra.isPermutationOf` List.map (evaluateTerm env) right
    Or subcons ->
      List.any (evaluateConstraint env) subcons


{-| Evaluate a term under a given variable assignment.
-}
evaluateTerm : EveryDict var Int -> Term var -> Int
evaluateTerm env term =
  case term of
    Constant val ->
      val
    Variable var ->
      case EveryDict.get var env of
        Just val -> val
        Nothing -> Debug.crash <| "can't evaluate undefined variable " ++ toString var
    Add terms ->
      List.sum <| List.map (evaluateTerm env) terms


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
