module Solver.Constraint exposing
  ( Term (..)
  , constant
  , variable

  , Constraint (..)
  , equal
  )

{-| Integer constraints over variables.

This module exposes both a low-level (type constructors) and high-level
(functions) interface for constraints.  It's recommended that clients
should stick with the high-level interface, which is more flexible.
The low-level interface is intended for constraint solver implementations.
-}


{-| A value term that appears in a constraint.
-}
type Term var
  = Constant Int
  | Variable var


{-| A constant value.
-}
constant : Int -> Term var
constant = Constant


{-| A variable.
-}
variable : var -> Term var
variable = Variable


{-| A constraint to be satisfied.

Whenever a constraint implements a relation R, the interpretation is
"a R b".  For example, LessThan a b would be "a is less than b".
-}
type Constraint var
  = Equal (Term var) (Term var)


{-| Constrain two terms to be exactly equal.
-}
equal : Term var -> Term var -> Constraint var
equal = Equal
