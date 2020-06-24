module SubpatternTests where

import Data.Either.Extra (fromRight)
import Test.QuickCheck.Regex.PCRE.Types
import Test.QuickCheck.Regex.PCRE
import Test.Tasty
import Test.Tasty.HUnit

subpatternTests :: [TestTree]
subpatternTests =
  [ testCase
      "numSubpatterns in ((a(b))"
      test_count_subpattern,
    testCase
      "numSubpatterns in ()()"
      test_count_empty_subpatterns
  ]
  where
    test_count_subpattern :: Assertion
    test_count_subpattern =
      assertEqual
        "miscounted the number of subpatterns in this regex"
        3
        ( numSubpatterns
            . fromRight
              ( Regex
                  ( Alternative [] []
                  )
              )
            $ parseRegex "((a(b)))"
        )
    test_count_empty_subpatterns :: Assertion
    test_count_empty_subpatterns =
      assertEqual
        "miscounted the number of subpatterns in this regex"
        2
        ( numSubpatterns
            . fromRight
              ( Regex
                  ( Alternative [] []
                  )
              )
            $ parseRegex "()()"
        )
