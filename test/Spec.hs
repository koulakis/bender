module Spec where

import Lib (test_function)
import Test.HUnit

test_test = TestCase $ assertEqual "Parsing \"100\"" 1001 (test_function "100")
test1 = TestCase $ assertEqual "One more test" 101 (test_function "10")

specs = TestList [TestLabel "test-test" test_test,
                  TestLabel "test1" test1]
