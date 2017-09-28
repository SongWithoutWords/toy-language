module AstBuilderT where

import Ast

eVarT :: Type -> Name -> ExprT
eVarT t = ExprT t . EVar

eLamT :: Type -> Named Type -> ExprT -> ExprT
eLamT t nt e = ExprT t $ ELamT nt e

eAppT :: Type -> ExprT -> ExprT -> ExprT
eAppT t a b = ExprT t $ EApp a b

eIfT :: Type -> PredT -> ExprT -> ExprT -> ExprT
eIfT t a b c = ExprT t $ EIf a b c

eBinOpT :: Type -> Op -> ExprT -> ExprT -> ExprT
eBinOpT t op a b = ExprT t $ EBinOp op a b

eIntT :: Int -> ExprT
eIntT i = ExprT TInt $ EVal $ VInt i

eBlnT :: Bool -> ExprT
eBlnT b = ExprT TBln $ EVal $ VBln b

