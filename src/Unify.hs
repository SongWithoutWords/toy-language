module Unify
  ( unifyConstraints
  , module Constraint
  , module Substitution
  ) where

import Control.Monad.Writer
import qualified Data.Map as M

import Constraint
import Substitution

type UnifyM = Writer [Constraint] -- a writer of unsolved constraints

deferConstraint :: Constraint -> UnifyM ()
deferConstraint c = tell [c]

unifyConstraints :: [Constraint] -> Substitutions
unifyConstraints [] = M.empty
unifyConstraints constraints =
  let (subs, deferredConstraints) = runWriter $ unifyConstraints' constraints in
  let deferredConstraints' = map (subConstraint subs) deferredConstraints in
  if deferredConstraints' == constraints
  then error $ "Failed to unify constraints" ++ show constraints
  else M.union subs $ unifyConstraints deferredConstraints'

unifyConstraints' :: [Constraint] -> UnifyM Substitutions
unifyConstraints' [] = pure M.empty
unifyConstraints' (c : cs) = do
  s2 <- unifyConstraints' cs
  s1 <- unifyConstraint $ subConstraint s2 c
  let s2' = M.map (subType s1) s2
  pure $ M.union s1 s2'

unifyConstraint :: Constraint -> UnifyM Substitutions
unifyConstraint (a := b)
  | a == b = pure M.empty

  | TVar x <- a = pure $ M.singleton x b

  | TVar y <- b = pure $ M.singleton y a

  | TFunc x1 x2 <- a
  , TFunc y1 y2 <- b
    = unifyConstraints' [x1 := y1, x2 := y2]

  | TFunc x1 x2 <- a
  , TOver overloads <- b
    = unifyWithOverload (x1 :# x2) overloads

  | TOver overloads <- a
  , TFunc x1 x2 <- b
    = unifyWithOverload (x1 :# x2) overloads

  | otherwise = error $ "cannot unify " ++ show a ++ " with " ++ show b

unifyWithOverload :: (Pair Type) -> [Pair Type] -> UnifyM Substitutions
unifyWithOverload pair@(x1 :# x2) overloads =
  let viableOverloads = filter (possibleToUnifyPairs pair) overloads in
  case viableOverloads of
    [] -> error $ "cannot unify " ++ show pair ++ " with overloads " ++ show overloads
    [y1 :# y2] -> unifyConstraints' [x1 := y1, x2 := y2]
    _ -> case x1 of
      TVar _ -> (deferConstraint $ TFunc x1 x2 := TOver overloads) >> pure M.empty
      _ -> error $ "cannot unify " ++ show pair ++ " with overlapping overloads " ++ show overloads

possibleToUnifyPairs :: (Pair Type) -> (Pair Type) -> Bool
possibleToUnifyPairs (x1 :# x2) (y1 :# y2) =
  possibleToUnifyType x1 y1 && possibleToUnifyType x2 y2

possibleToUnifyType :: Type -> Type -> Bool
possibleToUnifyType a b
  | a == b = True

  | TVar _ <- a = True -- it is possible

  | TVar _ <- b = True -- it is possible

  | TFunc x1 x2 <- a
  , TFunc y1 y2 <- b
    = possibleToUnifyType x1 y1 && possibleToUnifyType x2 y2

  | otherwise = False

subConstraint :: Substitutions -> Constraint -> Constraint
subConstraint = mapConstraint . subType

