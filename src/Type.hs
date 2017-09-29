module Type where

data Type
  = TLam Type Type
  | TBln
  | TInt
  | TVar Word
  | TError
  deriving(Eq, Show)

