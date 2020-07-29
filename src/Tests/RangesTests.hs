module RangesTests where

import Data.Maybe (isJust, isNothing)
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

rangesTests :: [TestTree]
rangesTests =
  [ testProperty "create ordered int range" prop_ordered_int,
    testCase "do not create misordered range" test_unordered_int,
    testCase "create ordered char range" test_ordered_char,
    testCase "create positive order range" test_positive_ordered
  ]

prop_ordered_int :: Int -> Int -> Bool
prop_ordered_int a b =
  if a <= b
    then isJust range
    else isNothing range
  where
    range = orderedRange a b

test_unordered_int :: Assertion
test_unordered_int =
  assertBool
    "should not have created a range with backwards order"
    (isNothing $ orderedRange (3 :: Int) (1 :: Int))

test_ordered_char :: Assertion
test_ordered_char =
  assertBool
    "could not create ordered range"
    (isJust $ orderedRange ('a' :: Char) ('b' :: Char))

test_positive_ordered :: Assertion
test_positive_ordered =
  assertBool
    "could not create positive ordered range"
    (isJust $ positiveOrderedRange (1 :: Int) (2 :: Int))

test_negative_ordered :: Assertion
test_negative_ordered =
  assertBool
    "should not have created a range with backwards order"
    (isNothing $ orderedRange (-2 :: Int) (-1 :: Int))
