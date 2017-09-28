module Unify
  ( unifyConstraints
  ) where

import Data.Map as M

import Constraint
import Substitution

unifyConstraints :: [Constraint] -> Substitutions
unifyConstraints [] = M.empty
unifyConstraints (c : cs) =
  let s2 = unifyConstraints cs in
  let s1 = unifyConstraint $ subConstraint s2 c in
    M.union s1 s2

unifyConstraint :: Constraint -> Substitutions
unifyConstraint (Constraint t1 t2) = let
  unify' :: Type -> Type -> Substitutions
  unify' a b

    | a == b = M.empty

    | TVar x <- a = M.singleton x b

    | TVar y <- b = M.singleton y a

    | TLam x1 x2 <- a
    , TLam y1 y2 <- b
    = unifyConstraints [Constraint x1 y1, Constraint x2 y2]

    | otherwise = error $ "cannot unify " ++ show a ++ " " ++ show b

  in unify' t1 t2

subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType


