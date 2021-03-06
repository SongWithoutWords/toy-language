{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}

module Ast
  ( module Ast
  , module Name
  , module Type
  ) where

import Name
import Type

data Typing
  = U -- untyped
  | T -- typed

type AstU = Ast 'U
type AstT = Ast 'T
data Ast t where
  Ast :: [Named (Expr t)] -> Expr t -> Ast t

deriving instance Eq(Ast t)
deriving instance Show(Ast t)


type ExprU = Expr 'U
type ExprT = Expr 'T
data Expr t where
  ExprU :: ExprU' -> ExprU
  ExprT :: Type -> ExprT' -> ExprT

deriving instance Eq(Expr t)
deriving instance Show(Expr t)


type ExprU' = Expr' 'U
type ExprT' = Expr' 'T
data Expr' t where
  EVar :: Name -> Expr' t

  -- Add multiple parameters later, as needed
  ELamU :: Name -> ExprU -> ExprU'
  ELamT :: Named Type -> ExprT -> ExprT'

  EApp :: Expr t -> Expr t -> Expr' t

  EIf :: Pred t -> Expr t -> Expr t -> Expr' t
  EBinOp :: Op -> Expr t -> Expr t -> Expr' t
  EVal :: Val -> Expr' t

deriving instance Eq(Expr' t)
deriving instance Show(Expr' t)

type PredU = Pred 'U
type PredT = Pred 'T
newtype Pred t = Pred (Expr t)

deriving instance Eq(Pred t)
deriving instance Show(Pred t)


data Op
  = Add
  | Sub
  | Mul
  | Div
  | LessThan
  | GreaterThan
  | And
  | Or
  deriving(Eq, Show)

data Val
  = VBln Bool
  | VInt Int
  deriving(Eq, Show)

