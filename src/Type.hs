module Type
  ( module Type
  , NonEmpty(..)
  , Pair(..)
  ) where

import Data.List.NonEmpty
import Data.UniformPair

data Type
  = TFunc Type Type
  | TOver [Pair Type] -- overloaded functions
  | TBln
  | TInt
  | TVar Word
  | TError
  deriving(Eq, Ord, Show)

