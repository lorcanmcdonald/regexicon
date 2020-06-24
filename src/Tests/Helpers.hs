module Helpers where

import Test.Tasty.HUnit
import Test.QuickCheck.Regex.PCRE

shouldBe :: String -> Regex -> Assertion
shouldBe s regex =
  assertEqual
    "Incorrectly parsed pattern"
    (Right regex)
    (parseRegex s)
