module Transforms
  ( module Transforms
  , module Ast
  , module ConstraintGen
  , module SubAst
  , module Unify
  ) where

import Ast
import ConstraintGen
import SubAst
import Unify

typeCheckAst :: AstU -> AstT
typeCheckAst ast =
  let (ast', constraints) = constrainAst ast in
  let substitutions = unifyConstraints constraints in
  subAst substitutions ast'

