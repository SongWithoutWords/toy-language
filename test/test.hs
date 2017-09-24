import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests -- putStrLn "Test suite not yet implemented"

tests :: TestTree
tests = testGroup "Tests" [testCase "id 1 == 1" $ (id 1) @?= 1]


