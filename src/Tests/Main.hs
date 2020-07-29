{-# LANGUAGE OverloadedStrings #-}

module Main where

import BackslashPatternTests
import MatchingTests
import ParseTests
import RangesTests
-- import RenderTests
import SubpatternTests
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ testGroup "Backslash patterns" backslashPatterns,
      testGroup "Matching" matchingTests,
      testGroup "Parse" parseTests,
      testGroup "Ranges" rangesTests,
      -- testGroup "Rendering" renderTests,
      testGroup "Subpatterns" subpatternTests
    ]
