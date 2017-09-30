module Test.Unify(unifyTests) where

import Data.Map as M

import Test.Tasty
import Test.Tasty.HUnit

import Unify

unifyTests :: TestTree
unifyTests = testGroup "Unification"
  [ test00
  , test01
  , test02
  , test03
  , test04
  , test05
  , test06
  , test07
  ]

unifyTest :: String -> [Constraint] -> Substitutions -> TestTree
unifyTest name c s = testCase name $ unifyConstraints c @?= s

test00 = unifyTest "00 - empty" c s
  where
    c = []; s = M.empty

test01 = unifyTest "01 - single" c s
  where
    c = [TVar 0 := TInt]
    s = M.singleton 0 TInt

test02 = unifyTest "02 - simple sub" c s
  where
    c = [TVar 0 := TVar 1, TVar 1 := TInt]
    s = M.fromList [(0, TInt), (1, TInt)]

test03 = unifyTest "03 - multiple subs" c s
  where
    c = [TVar 0 := TVar 1, TVar 1 := TVar 2, TVar 2 := TInt]
    s = M.fromList [(0, TInt), (1, TInt), (2, TInt)]

test04 = unifyTest "04 - lambda" c s
  where
    c = [TVar 0 := TLam (TVar 1) (TVar 2), TVar 1 := TVar 2, TVar 2 := TInt]
    s = M.fromList [(0, TLam TInt TInt), (1, TInt), (2, TInt)]

test05 = unifyTest "05 - lambda equality one sided" c s
  where
    c = [TLam (TVar 0) (TVar 1) := TLam TBln TInt]
    s = M.fromList [(0, TBln), (1, TInt)]

test06 = unifyTest "06 - lambda two sided" c s
  where
    c = [TLam (TVar 0) TInt := TLam TBln (TVar 1)]
    s = M.fromList [(0, TBln), (1, TInt)]

test07 = unifyTest "06 - typeVar := lambda" c s
  where
    c = [ TVar 0 := TLam (TVar 1) TInt
        , TVar 0 := TLam TBln (TVar 2) ]
    s = M.fromList [(0, TLam TBln TInt), (1, TBln), (2, TInt)]

