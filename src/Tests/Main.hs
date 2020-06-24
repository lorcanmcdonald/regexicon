{-# LANGUAGE OverloadedStrings #-}

module Main where

import BackslashPatternTests
import MatchingTests
import ParseTests
import SubpatternTests
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ testGroup "Parse" parseTests,
      testGroup "Subpatterns" subpatternTests,
      testGroup "Backslash patterns" backslashPatterns,
      testGroup "Matching" matchingTests
    ]
