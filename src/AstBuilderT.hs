module AstBuilderT where

import Ast

eVarT :: Type -> Name -> ExprT
eVarT t = ExprT t . EVar

eLamU :: Type -> Named Type -> ExprT -> ExprT
eLamU t nt e = ExprT t $ ELamT nt e

eAppU :: Type -> ExprT -> ExprT -> ExprT
eAppU t a b = ExprT t $ EApp a b

eIfU :: Type -> PredT -> ExprT -> ExprT -> ExprT
eIfU t a b c = ExprT t $ EIf a b c

eBinOpU :: Type -> Op -> ExprT -> ExprT -> ExprT
eBinOpU t op a b = ExprT t $ EBinOp op a b

eIntU :: Int -> ExprT
eIntU i = ExprT TInt $ EVal $ VInt i

eBlnU :: Bool -> ExprT
eBlnU b = ExprT TBln $ EVal $ VBln b

