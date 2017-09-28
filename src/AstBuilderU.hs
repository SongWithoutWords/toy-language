module AstBuilderU where

import Ast

eVarU :: Name -> ExprU
eVarU = ExprU . EVar

eLamU :: Name -> ExprU -> ExprU
eLamU n e = ExprU $ ELamU n e

eAppU :: ExprU -> ExprU -> ExprU
eAppU a b = ExprU $ EApp a b

eIfU :: PredU -> ExprU -> ExprU -> ExprU
eIfU a b c = ExprU $ EIf a b c

eBinOpU :: Op -> ExprU -> ExprU -> ExprU
eBinOpU op a b = ExprU $ EBinOp op a b

eIntU :: Int -> ExprU
eIntU i = ExprU $ EVal $ VInt i

eBlnU :: Bool -> ExprU
eBlnU b = ExprU $ EVal $ VBln b

