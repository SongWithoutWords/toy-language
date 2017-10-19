module Substitution where

import qualified Data.Map as M

import Type

type Substitutions = M.Map Word Type

subType :: Substitutions -> Type -> Type
subType s (TFunc t1 t2) = TFunc (subType s t1) (subType s t2)
subType _ TBln = TBln
subType _ TInt = TInt
subType _ TError = TError
subType s (TVar tvar) = case M.lookup tvar s of
  Nothing -> TVar tvar
  Just t -> t

