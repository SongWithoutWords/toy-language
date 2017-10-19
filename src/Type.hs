module Type where

data Type
  = TFunc FType
  | TBln
  | TInt
  | TVar Word
  | TError
  deriving(Eq, Show)

data FType = FType Type Type
  deriving(Eq, Show)

