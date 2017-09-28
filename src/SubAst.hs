{-# language GADTs #-}

module SubsAst
  ( subAst
  ) where

import Ast
import Substitution

subAst :: Substitutions -> AstT -> AstT
subAst s (Ast namedExprs expr) =
  Ast (map (fmap $ subExpr s) namedExprs) (subExpr s expr)

subExpr :: Substitutions -> ExprT -> ExprT
subExpr s (ExprT t e) = ExprT (subsType s t) (subExpr' s e)

subExpr' :: Substitutions -> ExprT' -> ExprT'
subExpr' s expr = let sub = subExpr s in case expr of
  EVar n -> EVar n
  ELamT nt e -> ELamT nt $ sub e
  EApp e1 e2 -> EApp (sub e1) (sub e2)
  EIf (Pred e1) e2 e3 -> EIf (Pred $ sub e1) (sub e2) (sub e3)
  EBinOp op e1 e2 -> EBinOp op (sub e1) (sub e2)
  EVal v -> EVal v

