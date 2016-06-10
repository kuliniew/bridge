module Constraint exposing
  ( Range
  , Term (..)
  , Constraint (..)
  , State

  , initialize
  , range
  , possibleValues
  , constrain
  , guaranteed
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
  | Multiply (List (Term var))
  | Max (List (Term var))
  | Negate (Term var)


{-| An individual constraint over terms.
-}
type Constraint var
  = LessThan (Term var) (Term var)
  | GreaterThan (Term var) (Term var)
  | Minimum (Term var) (Term var)
  | Maximum (Term var) (Term var)
  | Equal (Term var) (Term var)
  | Permutation (List (Term var)) (List (Term var))
  | Or (List (Constraint var))
  | And (List (Constraint var))
  | Not (Constraint var)
  | Null


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
    vars = variables (Debug.log "new constraint" constraint)
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
        GreaterThan left right -> [left, right]
        Minimum left right -> [left, right]
        Maximum left right -> [left, right]
        Equal left right -> [left, right]
        Permutation left right -> left ++ right
        Or subcons -> List.concatMap termsInConstraint subcons
        And subcons -> List.concatMap termsInConstraint subcons
        Not subcon -> termsInConstraint subcon
        Null -> []
    variablesInTerm term =
      case term of
        Constant _ -> []
        Variable var -> [var]
        Add terms -> List.concatMap variablesInTerm terms
        Multiply terms -> List.concatMap variablesInTerm terms
        Max terms -> List.concatMap variablesInTerm terms
        Negate subterm -> variablesInTerm subterm
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
          Debug.log "changed variables" <| changedVariables state state'
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
  let
    extractVar term =
      case term of
        Variable var -> Just var
        _ -> Nothing
    allVars terms =
      let
        found = List.filterMap extractVar terms
      in
        if List.length found == List.length terms
        then Just found
        else Nothing
  in
    case constraint of
      Equal (Constant sum) (Add terms) ->
        case allVars terms of
          Just vars -> enforceVariableSumEquality sum vars state
          Nothing -> enforceConstraintNaive constraint state
      Equal (Add terms) (Constant sum) ->
        case allVars terms of
          Just vars -> enforceVariableSumEquality sum vars state
          Nothing -> enforceConstraintNaive constraint state
      _ ->
        enforceConstraintNaive constraint state


{-| Enforce a list of variables to add up to a specific value.
-}
enforceVariableSumEquality : Int -> List var -> State var -> State var
enforceVariableSumEquality sum vars state =
  let
    minSum =
      List.sum <| List.filterMap (flip smallestValue state) vars
    maxSum =
      List.sum <| List.filterMap (flip largestValue state) vars
    (State st) =
      state
    keep smallest largest value =
      value + (minSum - smallest) <= sum && value + (maxSum - largest) >= sum
    updateVar var vars =
      case (possibleValues var state, smallestValue var state, largestValue var state) of
        (range, Just smallest, Just largest) ->
          EveryDict.insert var (Set.filter (keep smallest largest) range) vars
        _ ->
          vars
  in
    State { st | variables = List.foldl updateVar st.variables vars }


{-| Enforce a single constraint using a naive but general algorithm.
-}
enforceConstraintNaive : Constraint var -> State var -> State var
enforceConstraintNaive constraint (State st) =
  let
    spy tag xs =
      let
        _ = Debug.log tag (List.length xs)
      in
        xs
    vars =
      DictSet.fromList toString <| variables (Debug.log "enforcing" constraint)
    updatedVariables =
      st.variables
        |> EveryDict.filter (\var _ -> DictSet.member var vars)
        |> Debug.log "taking cartesian product of"
        |> cartesianProduct
        |> spy "before filtering"
        |> List.filter (flip evaluateConstraint constraint)
        |> spy "after filtering"
        |> unCartesianProduct (DictSet.toList vars)
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
          [EveryDict.empty]
        (var, values) :: rest ->
          let
            assignments = List.map (\val -> (var, val)) (Set.toList values)
          in
            List.concatMap (\others -> List.map (\(var, val) -> EveryDict.insert var val others) assignments) (expand rest)
  in
    expand <| EveryDict.toList vars


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
    GreaterThan left right ->
      evaluateTerm env left > evaluateTerm env right
    Minimum left right ->
      evaluateTerm env left >= evaluateTerm env right
    Maximum left right ->
      evaluateTerm env left <= evaluateTerm env right
    Equal left right ->
      evaluateTerm env left == evaluateTerm env right
    Permutation left right ->
      List.map (evaluateTerm env) left `List.Extra.isPermutationOf` List.map (evaluateTerm env) right
    Or subcons ->
      List.any (evaluateConstraint env) subcons
    And subcons ->
      List.all (evaluateConstraint env) subcons
    Not subcon ->
      not <| evaluateConstraint env subcon
    Null ->
      True


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
    Multiply terms ->
      List.product <| List.map (evaluateTerm env) terms
    Max terms ->
      case List.maximum <| List.map (evaluateTerm env) terms of
        Just result -> result
        Nothing -> Debug.crash "tried to take Max of an empty list"
    Negate subterm ->
      negate <| evaluateTerm env subterm


{-| Find the variables that have different ranges between two
snapshots of the state.
-}
changedVariables : State var -> State var -> List var
changedVariables (State oldSt) (State newSt) =
  let
    values var state =
      case EveryDict.get var state.variables of
        Just set -> Set.toList set
        Nothing -> []
  in
    EveryDict.keys oldSt.variables
      |> List.filter (\var -> values var oldSt /= values var newSt)


{-| Take an arbitrary item out of a work list.
-}
takeWorkItem : DictSet comparable a -> Maybe (a, DictSet comparable a)
takeWorkItem worklist =
  case DictSet.toList worklist of
    [] ->
      Nothing
    item :: rest ->
      Just (item, DictSet.remove item worklist)


{-| Check if a constraint is consistent with existing constraints.
-}
possible : Constraint var -> State var -> Bool
possible constraint state =
  let
    (State newSt) = constrain constraint state
  in
    List.all (not << Set.isEmpty) <| EveryDict.values newSt.variables


{-| Check if a constraint is guaranteed to be satisfied by existing
constraints.
-}
guaranteed : Constraint var -> State var -> Bool
guaranteed constraint state =
  not <| possible (Not constraint) state
