{-# language DataKinds #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}

module Ast where

import Type

data Typing
  = U -- untyped
  | T -- typed

type Name = String

data Named a = Named Name a
  deriving(Eq, Show)

data Program t where
  Program :: [Named (Expr t)] -> Expr t -> Program t

deriving instance Eq(Program t)
deriving instance Show(Program t)


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
  ELam :: Name -> Expr t -> Expr' t
  EApp :: Expr t -> Expr t -> Expr' t

  EIf :: Expr t -> Expr t -> Expr t -> Expr' t
  EBinOp :: Op -> Expr t -> Expr t -> Expr' t
  EVal :: Val -> Expr' t

deriving instance Eq(Expr' t)
deriving instance Show(Expr' t)


data Op
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  deriving(Eq, Show)

data Val
  = VBln Bool
  | VInt Int
  deriving(Eq, Show)

