module Constraint
  ( module Constraint
  , module Type
  ) where

import Type

data Constraint
  = Type := Type
  deriving(Eq, Show)

mapConstraint :: (Type -> Type) -> Constraint -> Constraint
mapConstraint f (t1 := t2) = (f t1) := (f t2)

