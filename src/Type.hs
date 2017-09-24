module Type where

data Type
  = TLam Type Type
  | TBln
  | TInt
  | TVar TypeVar
  | TError
  deriving(Eq, Show)

newtype TypeVar = TypeVar Word
  deriving(Eq, Show)

