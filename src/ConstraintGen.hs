{-# language GADTs #-}

module ConstraintGen
  ( constrain
  ) where

import Control.Monad.RWS

import Ast
import Constraint
import Type

constrain :: AstU -> (AstT, [Constraint])
constrain = undefined

type ConstrainM a = RWS AstT [Constraint] TypeVar a

nextTypeVar :: ConstrainM TypeVar
nextTypeVar = do
  var@(TypeVar val) <- get
  put $ TypeVar (val + 1)
  pure var

nextType :: ConstrainM Type
nextType = nextTypeVar >>= pure . TVar

constraint :: Type -> Type -> ConstrainM ()
constraint t1 t2 = tell [Constraint t1 t2]

checkAst :: AstU -> ConstrainM AstT
checkAst (Ast exprs expr) = do
  exprs' <- traverse checkNamedExpr exprs
  expr' <- checkExpr expr
  pure $ Ast exprs' expr'

checkNamedExpr :: Named ExprU -> ConstrainM (Named ExprT)
checkNamedExpr = traverse checkExpr

checkExpr :: ExprU -> ConstrainM ExprT
checkExpr (ExprU expr) = do
  thisType <- nextType
  expr' <- case expr of

    EIf e1 cn e2 -> do
      e1'@(ExprT t1 _) <- checkExpr e1
      cn'@(ExprT tc _) <- checkExpr cn
      e2'@(ExprT t2 _) <- checkExpr e2

      constraint thisType t1
      constraint TBln tc
      constraint thisType t2

      pure $ EIf e1' cn' e2'


  pure $ ExprT thisType expr'


