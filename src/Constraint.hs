module Constraint where

import Type

data Constraint
  = Constraint Type Type
  deriving(Eq, Show)
