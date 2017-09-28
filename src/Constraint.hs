module Constraint
  ( module Constraint
  , module Type
  ) where

import Type

data Constraint
  = Constraint Type Type
  deriving(Eq, Show)

mapConstraint :: (Type -> Type) -> Constraint -> Constraint
mapConstraint f (Constraint t1 t2) = Constraint (f t1) (f t2)

