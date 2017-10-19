module Unify
  ( unifyConstraints
  , module Constraint
  , module Substitution
  ) where

import qualified Data.Map as M

import Constraint
import Substitution

unifyConstraints :: [Constraint] -> Substitutions
unifyConstraints [] = M.empty
unifyConstraints (c : cs) =
  let s2 = unifyConstraints cs in
  let s1 = unifyConstraint $ subConstraint s2 c in
  let s2' = M.map (subType s1) s2 in
    M.union s1 s2'

unifyConstraint :: Constraint -> Substitutions
unifyConstraint (a := b)
  | a == b = M.empty

  | TVar x <- a = M.singleton x b

  | TVar y <- b = M.singleton y a

  | TFunc x1 x2 <- a
  , TFunc y1 y2 <- b
    = unifyConstraints [x1 := y1, x2 := y2]

  | TFunc x1 x2 <- a
  , TOver overloads <- b
    = unifyWithOverload (x1 :# x2) overloads

  | TOver overloads <- a
  , TFunc x1 x2 <- b
    = unifyWithOverload (x1 :# x2) overloads

  | otherwise = error $ "cannot unify " ++ show a ++ " with " ++ show b

unifyWithOverload :: (Pair Type) -> [Pair Type] -> Substitutions
unifyWithOverload pair@(x1 :# x2) overloads =
  let viableOverloads = filter (pairsUnifiable pair) overloads in
  case viableOverloads of
    [] -> error $ "cannot unify " ++ show pair ++ " with overloads " ++ show overloads
    [y1 :# y2] -> unifyConstraints [x1 := y1, x2 := y2]
    _ -> error $ "cannot unify " ++ show pair ++ " with overlapping overloads " ++ show overloads

  where
    pairsUnifiable :: (Pair Type) -> (Pair Type) -> Bool
    pairsUnifiable (x1 :# x2) (y1 :# y2) =
      typesUnifiable x1 y1 && typesUnifiable x2 y2

    typesUnifiable :: Type -> Type -> Bool
    typesUnifiable a b
      | a == b = True

      | TVar _ <- a = True

      | TVar _ <- b = True

      | TFunc x1 x2 <- a
      , TFunc y1 y2 <- b
        = typesUnifiable x1 y1 && typesUnifiable x2 y2

      | otherwise = False
  



subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType



