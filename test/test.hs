import Test.Tasty
import Test.Tasty.HUnit

import AstBuilderU
import AstBuilderT
import Transforms

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "type-check fact"
    $ typeCheckAst factorialImplicit @?= factorialExplicit
  ]

factorialImplicit :: AstU
factorialImplicit = Ast
  [ Named "fact" $ eLamU "n" $
    eIfU
      (Pred $ eBinOpU LessThan (eVarU "n") $ eIntU 1)
      (eIntU 1)
      (eBinOpU Mul
        (eVarU "n")
        (eAppU (eVarU "fact")
          (eBinOpU Sub (eVarU "n") $ eIntU 1)))]
  (eAppU (eVarU "fact") (eIntU 5))

factorialExplicit :: AstT
factorialExplicit = Ast
  [ Named "fact" $ eLamT (TLam TInt TInt) (Named "n" TInt) $
    eIfT TInt
      (Pred $ eBinOpT TBln LessThan (eVarT TInt "n") $ eIntT 1)
      (eIntT 1)
      (eBinOpT TInt Mul
        (eVarT TInt "n")
        (eAppT TInt (eVarT (TLam TInt TInt) "fact")
          (eBinOpT TInt Sub (eVarT TInt "n") $ eIntT 1)))]
  (eAppT TInt (eVarT (TLam TInt TInt) "fact") (eIntT 5))


