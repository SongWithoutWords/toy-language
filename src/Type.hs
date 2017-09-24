module Type where

data Type
  = TLam Type Type
  | TBln
  | TInt
  | TVar Word
  deriving(Eq, Show)

