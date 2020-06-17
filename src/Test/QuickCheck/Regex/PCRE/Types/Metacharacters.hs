{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickCheck.Regex.PCRE.Types.Metacharacters where

import Data.Data
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import {-# SOURCE #-} Test.QuickCheck.Regex.PCRE.Types.Quantifiable
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Test.QuickCheck.Regex.PCRE.RegexRenderer

data Metacharacter
  = ZeroOrMore Quantifiable
  | OneOrMore Quantifiable
  | MinMax Quantifiable (PositiveOrderedRange Int)
  deriving (Data, Eq, Generic, Show)

instance Arbitrary Metacharacter where
  arbitrary =
    oneof
      [ ZeroOrMore <$> arbitrary,
        OneOrMore <$> arbitrary,
        MinMax <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Exemplify Metacharacter where
  examples (ZeroOrMore q) = fmap concat . listOf $ examples q
  examples (OneOrMore q) = fmap concat . listOf1 $ examples q
  examples (MinMax q r) = do
    let (a, b) = extractPositiveRange r
    k <- choose (a, b)
    fmap concat . vectorOf k $ examples q

instance RegexRenderer Metacharacter where
  render (ZeroOrMore q) = render q <> "*"
  render (OneOrMore q) = render q <> "+"
  render (MinMax q range) = render q <> "{" <> show a <> "," <> show b <> "}"
    where
      (a, b) = extractPositiveRange range
