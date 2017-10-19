module Type where

import Data.List.NonEmpty
import Data.UniformPair

data Type
  = TFunc Type Type
  | TBln
  | TInt
  | TVar Word
  | TError
  deriving(Eq, Show)

