module RenderTests where

import Data.Either
import Test.QuickCheck.Regex.PCRE
import Test.Tasty
import Test.Tasty.QuickCheck

renderTests :: [TestTree]
renderTests =
  [ testProperty "parsing preserves rendering" prop_render_parse_render,
    testProperty "rendering is parsable" prop_parse_render
  ]

transitive :: String -> Bool
transitive s = case parseRegex s of
  Right re -> toText re == s
  Left _ -> False

prop_parse_render :: Regex -> Bool
prop_parse_render =
  isRight . parseRegex . toText

prop_render_parse_render :: Regex -> Bool
prop_render_parse_render re =
  ( toText
      . fromRight
        (error "toText produced unparsable regular expression")
      . parseRegex
      . toText
      $ re
  )
    == toText re
