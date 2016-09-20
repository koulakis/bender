module Spec where

import Lib (test_function)
import Test.HUnit

test_test = TestCase $ assertEqual "Parsing \"100\"" 100 (test_function "100")
