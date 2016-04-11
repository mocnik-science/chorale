module Chorale.Test.Common (
    tests) where

import Chorale.Common
import Chorale.Test

import Data.List
import Safe
import Test.Framework hiding (testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck.Property

-- --== Tests

tests :: [Test]
tests = [testGroupBool, testGroupCommon]

-- --== FBM.Common.Bool

testGroupBool :: Test
testGroupBool = testGroup "FBM.Common.Bool" [
        testCase "xor 1" caseXor1,
        testCase "xor 2" caseXor2,
        testCase "xor 3" caseXor3,
        testCase "xor 4" caseXor4,
        testCase "xnor 1" caseXnor1,
        testCase "xnor 2" caseXnor2,
        testCase "xnor 3" caseXnor3,
        testCase "xnor 4" caseXnor4
    ]

caseXor1 :: Assertion
caseXor1 = assertEqualBool "" False (xor True True)

caseXor2 :: Assertion
caseXor2 = assertEqualBool "" False (xor False False)

caseXor3 :: Assertion
caseXor3 = assertEqualBool "" True (xor True False)

caseXor4 :: Assertion
caseXor4 = assertEqualBool "" True (xor False True)

caseXnor1 :: Assertion
caseXnor1 = assertEqualBool "" True (xnor True True)

caseXnor2 :: Assertion
caseXnor2 = assertEqualBool "" True (xnor False False)

caseXnor3 :: Assertion
caseXnor3 = assertEqualBool "" False (xnor True False)

caseXnor4 :: Assertion
caseXnor4 = assertEqualBool "" False (xnor False True)

-- --== FBM.Common.Common

testGroupCommon :: Test
testGroupCommon = testGroup "FBM.Common.Common" [
        testProperty "lookupBy" propLookupBy,
        testProperty "takeWhileList" propTakeWhileList,
        testProperty "takeToFirst" propTakeToFirst,
        testProperty "splitOnFirst" propSplitOnFirst,
        testProperty "nubOrd" propNubOrd,
        testProperty "nubOrdBy'" propNubOrdBy',
        testProperty "replaceInList 1" propReplaceInList1,
        testProperty "replaceInList 2" propReplaceInList2,
        testProperty "replaceElementInList" propReplaceElementInList,
        testProperty "applyToList 1" propApplyToList1,
        testProperty "applyToList 2" propApplyToList2,
        testProperty "applyToList 3" propApplyToList3,
        testProperty "applyToList 4" propApplyToList4
    ]

-- @lookupBy f b xs@ finds the element @x@ which satisfies @f x = b@
propLookupBy :: Int -> Int -> [Int] -> Bool
propLookupBy fn b xs = let
        l = lookupBy f b xs
        f = (* fn)
    in case l of
        Just l' -> f l' == b
        Nothing -> b `notElem` map f xs

-- | @takeWhile p == takeWhileList (p . head)@
propTakeWhileList :: Int -> Int -> [Int] -> Property
propTakeWhileList i j xs = (0 /= i && j < i) ==> takeWhile p xs == takeWhileList (p . last) xs where
    p x = x `mod` i == j

-- | @takeWhile (not . p) = init . takeToFirst $ p@ (apart from special cases)
propTakeToFirst :: Int -> Int -> [Int] -> Property
propTakeToFirst i j xs = (0 /= i && j < i && (not . null) xs) ==> takeWhile (not . p) xs == (l . takeToFirst p) xs where
    p x = x `mod` i == j
    l
        | any p xs = init
        | otherwise = id

-- | @((\(as, bs) -> as ++ [x] ++ bs) . propSplitOnFirst x) xs == xs@
propSplitOnFirst :: Int -> [Int] -> Bool
propSplitOnFirst x xs = ((\(as, bs) -> as ++ maybe [] ([x] ++) bs) . splitOnFirst x) xs == xs

-- | @nubOrd == nub@
propNubOrd :: [Int] -> Bool
propNubOrd = uncurry (==) . map21 (nubOrd, nub)

-- | @nubOrdBy' f = nubBy (equaling f)@
propNubOrdBy' :: Int -> [(Int, Int)] -> Bool
propNubOrdBy' i = uncurry (==) . map21 (nubOrdBy' fst, nubBy $ equaling fst)

-- | @replaceInList i [xs!!i] xs == xs@
propReplaceInList1 :: [Int] -> Int -> Property
propReplaceInList1 xs j = length xs > 0 ==> replaceInList i [xs!!i] xs == xs where
    i = j `mod` length xs

-- | @replaceInList i []@ just removes the i-th element
propReplaceInList2 :: [Int] -> Int -> Property
propReplaceInList2 xs j = (not . null) xs ==> replaceInList i [] xs == (uncurry (++) . mapSnd tail . splitAt i) xs where
    i = abs j `mod` length xs

-- | ensure that @replaceElementInList (xs!!i)@ and @replaceInList i@ are the same
propReplaceElementInList :: [Int] -> Int -> [Int] -> Property
propReplaceElementInList xs j as = (not . null) xs && nub xs == xs ==> replaceElementInList (xs!!i) as xs == replaceInList i as xs where
    i = abs j `mod` length xs

-- | ensure that the length of the list stays the same
propApplyToList1 :: Int -> Int -> [Int] -> Property
propApplyToList1 n m xs = (0 <= n && n < length xs) ==> length (applyToList n (* m) xs) == length xs

-- | ensure that the identity does not change the list
propApplyToList2 :: Int -> [Int] -> Property
propApplyToList2 n xs = (0 <= n && n < length xs) ==> applyToList n id xs == xs

-- | ensure that the function is applied to the list
propApplyToList3 :: Int -> Int -> [Int] -> Property
propApplyToList3 n m xs = (0 <= n && n < length xs) ==> ((`at` n) . applyToList n (* m)) xs == ((* m) . (`at` n)) xs

-- | ensure that the rest of the list is not changed
propApplyToList4 :: Int -> Int -> [Int] -> Property
propApplyToList4 n m xs = (0 <= n && n < length xs) ==> (replaceInList n [] . applyToList n (* m)) xs == replaceInList n [] xs
