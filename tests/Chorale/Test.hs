module Chorale.Test (
    -- * Test.Framework
    testGroup,
    -- * HUnit
    assertEqualBool,
    assertEqualDouble) where

import Control.Monad
import Data.AEq
import qualified Test.Framework as TF
import Test.HUnit

-- --== TEST.FRAMEWORK

testGroup :: TF.TestName -> [TF.Test] -> TF.Test
testGroup name = TF.testGroup ('\n' : name)

-- --== HUNIT

assertEqualBool :: String -> Bool -> Bool -> Assertion
assertEqualBool preface expected actual = unless (expected == actual) (assertFailure msg) where
    msg = (if null preface then "" else preface ++ "\n") ++ "expected: " ++ show expected ++ "\n but got: " ++ show actual

assertEqualDouble :: String -> Double -> Double -> Assertion
assertEqualDouble preface expected actual = unless (expected ~== actual) (assertFailure msg) where
    msg = (if null preface then "" else preface ++ "\n") ++ "expected: " ++ show expected ++ "\n but got: " ++ show actual
