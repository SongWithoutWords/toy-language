import Test.Tasty
import Test.Tasty.HUnit

import Test.Unify

import AstBuilderU
import AstBuilderT
import Transforms

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ unifyTests

  , testCase "var"
    $ typeCheckAst varImp @?= varExp

  , testCase "ordered sum"
    $ typeCheckAst sumOrderedImp @?= sumOrderedExp

  , testCase "unordered sum"
    $ typeCheckAst sumUnorderedImp @?= sumUnorderedExp

  , testCase "inc"
    $ typeCheckAst incImp @?= incExp

  , testCase "type-check fact"
    $ typeCheckAst factImp @?= facExp

  ]

varImp :: AstU
varImp = Ast [ Named "a" $ eIntU 5 ] $ eVarU "a"

varExp :: AstT
varExp = Ast [ Named "a" $ eIntT 5] $ eVarT TInt "a"

sumOrderedImp :: AstU
sumOrderedImp = Ast
  [ Named "a" $ eIntU 5
  , Named "b" $ eIntU 3
  , Named "c" $ eBinOpU Add (eVarU "a") (eVarU "b")
  ]
  $ eVarU "c"

sumOrderedExp :: AstT
sumOrderedExp = Ast
  [ Named "a" $ eIntT 5
  , Named "b" $ eIntT 3
  , Named "c" $ eBinOpT TInt Add (eVarT TInt "a") (eVarT TInt "b")
  ]
  $ eVarT TInt "c"

sumUnorderedImp :: AstU
sumUnorderedImp = Ast
  [ Named "a" $ eIntU 5
  , Named "c" $ eBinOpU Add (eVarU "a") (eVarU "b")
  , Named "b" $ eIntU 3
  ]
  $ eVarU "c"

sumUnorderedExp :: AstT
sumUnorderedExp = Ast
  [ Named "a" $ eIntT 5
  , Named "c" $ eBinOpT TInt Add (eVarT TInt "a") (eVarT TInt "b")
  , Named "b" $ eIntT 3
  ]
  $ eVarT TInt "c"

identityImp :: AstU
identityImp = Ast
  [ Named "id" $ eLamU "x"
    $ eVarU "x"
  ]
  (eAppU (eVarU "id") (eIntU 7))

incImp :: AstU
incImp = Ast
  [ Named "inc" $ eLamU "i"
    $ eBinOpU Add (eVarU "i") (eIntU 1)
  ]
  (eAppU (eVarU "inc") (eIntU 7))

incExp :: AstT
incExp = Ast
  [ Named "inc" $ eLamT (TFunc $ FType TInt TInt) (Named "i" TInt)
    $ eBinOpT TInt Add (eVarT TInt "i") (eIntT 1)
  ]
  (eAppT TInt (eVarT (TFunc $ FType TInt TInt) "inc") (eIntT 7))

factImp :: AstU
factImp = Ast
  [ Named "fact" $ eLamU "n" $
    eIfU
      (Pred $ eBinOpU LessThan (eVarU "n") $ eIntU 1)
      (eIntU 1)
      (eBinOpU Mul
        (eVarU "n")
        (eAppU (eVarU "fact")
          (eBinOpU Sub (eVarU "n") $ eIntU 1)))
  ]
  (eAppU (eVarU "fact") (eIntU 5))

facExp :: AstT
facExp = Ast
  [ Named "fact" $ eLamT (TFunc $ FType TInt TInt) (Named "n" TInt) $
    eIfT TInt
      (Pred $ eBinOpT TBln LessThan (eVarT TInt "n") $ eIntT 1)
      (eIntT 1)
      (eBinOpT TInt Mul
        (eVarT TInt "n")
        (eAppT TInt (eVarT (TFunc $ FType TInt TInt) "fact")
          (eBinOpT TInt Sub (eVarT TInt "n") $ eIntT 1)))
  ]
  (eAppT TInt (eVarT (TFunc $ FType TInt TInt) "fact") (eIntT 5))


