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
  , test08
  , test09
  , test10
  , test11
  , test12
  , test13
  , test14
  , test15
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
    c = [TVar 0 := TFunc (TVar 1) (TVar 2), TVar 1 := TVar 2, TVar 2 := TInt]
    s = M.fromList [(0, TFunc TInt TInt), (1, TInt), (2, TInt)]

test05 = unifyTest "05 - lambda equality one sided" c s
  where
    c = [TFunc (TVar 0) (TVar 1) := TFunc TBln TInt]
    s = M.fromList [(0, TBln), (1, TInt)]

test06 = unifyTest "06 - lambda two sided" c s
  where
    c = [TFunc (TVar 0) TInt := TFunc TBln (TVar 1)]
    s = M.fromList [(0, TBln), (1, TInt)]

test07 = unifyTest "07 - typeVar := lambda" c s
  where
    c = [ TVar 0 := TFunc (TVar 1) TInt
        , TVar 0 := TFunc TBln (TVar 2)]
    s = M.fromList [(0, TFunc TBln TInt), (1, TBln), (2, TInt)]

test08 = unifyTest "08 - inc constraints" c s
  where
    c = [ TInt := TVar 1
        , TInt := TInt
        , TInt := TVar 2
        , TVar 0 := TFunc (TVar 1) (TVar 2)
        , TVar 0 := TFunc TInt (TVar 3)
        ]

    s = M.fromList
        [ (0, TFunc  TInt TInt)
        , (1, TInt)
        , (2, TInt)
        , (3, TInt)
        ]

test09 = unifyTest "09 - singleton overload" c s
  where
    c = [ TFunc TBln (TVar 0) := TOver [TBln :# TBln] ]

    s = M.fromList [(0, TBln)]

test10 = unifyTest "10 - simple overload" c s
  where
    c = [ TFunc TBln (TVar 0) := TOver [TBln :# TBln, TInt :# TInt]
        , TFunc TInt (TVar 1) := TOver [TBln :# TBln, TInt :# TInt]
        ]

    s = M.fromList
        [ (0, TBln)
        , (1, TInt)
        ]

test11 = unifyTest "11 - overload with implicit return types" c s
  where
    c = [ TVar 0 := TInt
        , TVar 1 := TBln

        , TFunc TBln (TVar 2) := TOver [TBln :# TVar 0, TInt :# TVar 1]
        , TFunc TInt (TVar 3) := TOver [TBln :# TVar 0, TInt :# TVar 1]
        ]

    s = M.fromList
        [ (0, TInt)
        , (1, TBln)
        , (2, TInt)
        , (3, TBln)
        ]

test12 = unifyTest "12 - overload variable args, functions first" c s
  where
    c = [ TFunc (TVar 0) (TVar 1) := TOver [TBln :# TBln, TInt :# TInt]
        , TFunc (TVar 2) (TVar 3) := TOver [TBln :# TBln, TInt :# TInt]
        , TVar 0 := TBln
        , TVar 2 := TInt
        ]

    s = M.fromList
        [ (0, TBln)
        , (1, TBln)
        , (2, TInt)
        , (3, TInt)
        ]

test13 = unifyTest "13 - overload variable args, args first" c s
  where
    c =
      [ TVar 0 := TBln
      , TVar 2 := TInt
      , TFunc (TVar 0) (TVar 1) := TOver [TBln :# TBln, TInt :# TInt]
      , TFunc (TVar 2) (TVar 3) := TOver [TBln :# TBln, TInt :# TInt]
      ]

    s = M.fromList
      [ (0, TBln)
      , (1, TBln)
      , (2, TInt)
      , (3, TInt)
      ]

test14 = unifyTest "14 - pathological - overload with variable arg type" c s
  where
    c = [TFunc (TVar 0) TBln := TOver [TBln :# TBln, TInt :# TBln]]
    s = M.fromList []

test15 = unifyTest "15 - pathological - two overloads with variable arg types" c s
  where
    c =
      [ TFunc (TVar 0) TBln := TOver [TBln :# TBln, TInt :# TBln]
      , TFunc (TVar 1) TBln := TOver [TBln :# TBln, TInt :# TBln]]
    s = M.fromList []

