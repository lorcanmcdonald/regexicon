{-# LANGUAGE OverloadedStrings #-}

module Main where

import BackslashPatternTests
import MatchingTests
import ParseTests
import SubpatternTests
import Test.Tasty
import Test.Tasty.Ingredients.Rerun

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests =
  testGroup
    ""
    [ testGroup "Parse" parseTests,
      testGroup "Subpatterns" subpatternTests,
      testGroup "Backslash patterns" backslashPatterns,
      testGroup "Matching" matchingTests
    ]
