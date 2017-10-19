module Type where

import Data.List.NonEmpty
import Data.UniformPair

data Type
  = TFunc Type Type
  | TOver (NonEmpty (Pair Type)) -- overloaded functions
  | TBln
  | TInt
  | TVar Word
  | TError
  deriving(Eq, Show)

