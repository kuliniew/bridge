module Solver exposing
  ( Constraint
  , Problem
  , Range
  , Term

  , add
  , addConstraint
  , all
  , any
  , constant
  , empty
  , equal
  , greaterThan
  , greaterThanOrEqual
  , ifThen
  , ifThenElse
  , lessThan
  , lessThanOrEqual
  , multiply
  , not
  , or
  , possibleValues
  , range
  , singleton
  , subtract
  , sum
  , variable
  )

{-| Top-level module for the constraint solver, exposing the public interface.
-}

import Solver.Constraint
import Solver.Endpoint
import Solver.Interval
import Solver.Problem
import Solver.Range
import Solver.Term

import Maybe.Extra


type alias Constraint var =
  Solver.Constraint.Constraint var


type alias Problem var =
  Solver.Problem.Problem var


type alias Range =
  Solver.Range.Range


type alias Term var =
  Solver.Term.Term var


add : Term var -> Term var -> Term var
add =
  Solver.Term.add


addConstraint : Constraint var -> Problem var -> Problem var
addConstraint constraint problem =
  List.foldl Solver.Problem.addConstraint problem (Solver.Constraint.decompose constraint)


all : List (Constraint var) -> Constraint var
all =
  Solver.Constraint.all


any : List (Constraint var) -> Constraint var
any =
  Solver.Constraint.any


constant : Int -> Term var
constant =
  Solver.Term.constant


empty : Problem var
empty =
  Solver.Problem.empty


equal : Term var -> Term var -> Constraint var
equal =
  Solver.Constraint.equal


greaterThan : Term var -> Term var -> Constraint var
greaterThan =
  Solver.Constraint.greaterThan


greaterThanOrEqual : Term var -> Term var -> Constraint var
greaterThanOrEqual =
  Solver.Constraint.greaterThanOrEqual


ifThen : Constraint var -> Constraint var -> Constraint var
ifThen =
  Solver.Constraint.ifThen


ifThenElse : Constraint var -> Constraint var -> Constraint var -> Constraint var
ifThenElse =
  Solver.Constraint.ifThenElse


lessThan : Term var -> Term var -> Constraint var
lessThan =
  Solver.Constraint.lessThan


lessThanOrEqual : Term var -> Term var -> Constraint var
lessThanOrEqual =
  Solver.Constraint.lessThanOrEqual


multiply : Int -> Term var -> Term var
multiply =
  Solver.Term.multiply


not : Constraint var -> Constraint var
not =
  Solver.Constraint.not


or : Constraint var -> Constraint var -> Constraint var
or =
  Solver.Constraint.or


possibleValues : var -> Problem var -> Range
possibleValues =
  Solver.Problem.possibleValues


range : Int -> Int -> Range
range lo hi =
  Solver.Interval.fromEndpoints (Solver.Endpoint.Point lo) (Solver.Endpoint.Point hi)
    |> Maybe.Extra.maybeToList
    |> Solver.Range.fromIntervals


singleton : Int -> Range
singleton =
  Solver.Range.singleton


subtract : Term var -> Term var -> Term var
subtract =
  Solver.Term.subtract


sum : List (Term var) -> Term var
sum =
  Solver.Term.sum


variable : var -> Term var
variable =
  Solver.Term.variable
