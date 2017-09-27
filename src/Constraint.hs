module Constraint
  ( module Constraint
  , module Type
  ) where

import Type

data Constraint
  = Constraint Type Type
  deriving(Eq, Show)
