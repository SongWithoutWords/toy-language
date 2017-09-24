{-# language GADTs #-}

module ConstraintGen
  ( constrainAst
  ) where

import Control.Monad.RWS

import Ast
import Constraint
import Util

constrainAst :: AstU -> (AstT, [Constraint])
constrainAst ast =

  -- tie the knot, in order to refer to typevars further ahead in the input
  let (ast', _, constraints) = runRWS (checkAst ast) ast' initialState
  in (ast', constraints)

data ConstrainState = ConstrainState
  { localBindings :: [Named Type]
  , nextTypeVar :: TypeVar
  }

initialState :: ConstrainState
initialState = ConstrainState [] $ TypeVar 0

type ConstrainM a = RWS AstT [Constraint] ConstrainState a

pushLocal :: Named Type -> ConstrainM ()
pushLocal nt = modify $ \s -> s{localBindings = nt : localBindings s}

popLocal :: ConstrainM ()
popLocal = modify $ \s -> s{localBindings = tail $ localBindings s}

getNextTypeVar :: ConstrainM TypeVar
getNextTypeVar = do
  var@(TypeVar val) <- (gets nextTypeVar)
  modify $ \s -> s{nextTypeVar = TypeVar (val + 1)}
  pure var

nextType :: ConstrainM Type
nextType = getNextTypeVar >>= pure . TVar

constrain :: Type -> Type -> ConstrainM ()
constrain t1 t2 = tell [Constraint t1 t2]

checkAst :: AstU -> ConstrainM AstT
checkAst (Ast exprs expr) = do
  exprs' <- traverse checkNamedExpr exprs
  expr' <- checkExpr expr
  pure $ Ast exprs' expr'

checkNamedExpr :: Named ExprU -> ConstrainM (Named ExprT)
checkNamedExpr = traverse checkExpr

checkExpr :: ExprU -> ConstrainM ExprT
checkExpr (ExprU expression) = case expression of

  EVar name -> do
    locals <- gets localBindings
    (Ast globals _) <- ask

    let local = findMaybe
          (\(Named n t) -> if n == name then Just t else Nothing) locals

    let global = findMaybe
          (\(Named n (ExprT t _)) -> if n == name then Just t else Nothing) globals

    pure $ ExprT (local <?> global ?? TError) $ EVar name


  ELamU name expr -> do
    tLam <- nextType

    -- param'@(Named tParam _) <- checkParam param
    tParam <- nextType

    expr'@(ExprT tExpr _) <- checkExpr expr

    constrain tLam $ TLam tParam tExpr

    pure $ ExprT tLam $ ELamT (Named name tParam) expr'


  EApp e1 e2 -> do
    tApp <- nextType

    e1'@(ExprT t1 _) <- checkExpr e1
    e2'@(ExprT t2 _) <- checkExpr e2

    constrain t1 $ TLam t2 tApp

    pure $ ExprT tApp $ EApp e1' e2'

  EIf e1 cn e2 -> do
    tIf <- nextType

    e1'@(ExprT t1 _) <- checkExpr e1
    cn'@(ExprT tc _) <- checkExpr cn
    e2'@(ExprT t2 _) <- checkExpr e2

    constrain t1 tIf
    constrain tc TBln
    constrain t2 tIf

    pure $ ExprT tIf $ EIf e1' cn' e2'

  EBinOp op e1 e2 -> let

    checkBinOp :: Op -> Type -> Type -> Type -> ConstrainM ()

    checkBinOp = undefined

    in do
      e1'@(ExprT t1 _) <- checkExpr e1
      e2'@(ExprT t2 _) <- checkExpr e2

      tRes <- nextType
      checkBinOp op t1 t2 tRes

      pure $ ExprT tRes $ EBinOp op e1' e2'

  EVal v -> case v of
    b@VBln{} -> pure $ ExprT TBln $ EVal b
    i@VInt{} -> pure $ ExprT TInt $ EVal i

