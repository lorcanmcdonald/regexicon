module MatchingTests where

import Control.Monad (replicateM)
import Data.Either.Extra (fromRight)
import Test.QuickCheck
import Test.QuickCheck.Regex.PCRE
import Test.Tasty
import Test.Tasty.HUnit
import Data.List.NonEmpty (fromList)

matchingTests :: [TestTree]
matchingTests =
  [ testCase "\\d" test_matching_digit,
    testCase
      "(a(b))\\2\\1"
      test_nested_backreference,
    testCase
      "(a)(\\1)\\2"
      test_unnested_backreference,
    testCase
      "[0-9a-f]{32}"
      test_length_of_range
  ]

test_matching_digit :: Assertion
test_matching_digit = do
  s <- aString
  let b = all (\x -> x `elem` fmap (: "") ("0123456789" :: String)) s
  assertBool "Produced characters that shouldn't match" b
  where
    aString :: IO [String]
    aString = replicateM 10 . generate . matching $ aRegex
    aRegex :: Regex
    aRegex = Regex (Alternative . fromList $ [ RegexCharacterList [Quant (Backslash Digit)]])

test_nested_backreference :: Assertion
test_nested_backreference = do
  let regex = fromRight (error "Left") . parseRegex $ "(a(b))\\2\\1"
  only_example <- replicateM 1 . generate . matching $ regex
  assertEqual
    "did not generate the correct example for a back reference"
    "abbab"
    (head only_example)

test_unnested_backreference :: Assertion
test_unnested_backreference = do
  let regex = fromRight (error "Left") . parseRegex $ "(a)(\\1)\\2"
  only_example <- replicateM 1 . generate . matching $ regex
  assertEqual
    "did not generate the correct example for a back reference"
    "aaa"
    (head only_example)

test_length_of_range :: Assertion
test_length_of_range = do
  let regex = fromRight (error "Left") . parseRegex $ "[0-9a-f]{32}"
  rangeExample <- replicateM 10 . generate . matching $ regex
  assertBool
    "Produced an example which was not the specified length"
    (all ((== 32) . length) rangeExample)
