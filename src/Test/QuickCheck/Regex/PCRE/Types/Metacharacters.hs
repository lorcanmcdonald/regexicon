{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickCheck.Regex.PCRE.Types.Metacharacters where

import Data.Data
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import {-# SOURCE #-} Test.QuickCheck.Regex.PCRE.Types.Quantifiable
  ( Quantifiable (),
    backslashSequence,
  )
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Text.ParserCombinators.Parsec

data Metacharacter
  = ZeroOrMore Quantifiable
  | OneOrMore Quantifiable
  | MinMax Quantifiable (PositiveOrderedRange Int)
  deriving (Data, Eq, Generic, Show)

bsZero :: Quantifiable
bsZero = case parse backslashSequence "" "\\x" of
  Left s -> error ("Developer error: `bsZero` cannot be parsed.\n\t" <> show s)
  Right x -> x

instance Arbitrary Metacharacter where
  arbitrary =
    oneof
      [ ZeroOrMore <$> arbitrary,
        OneOrMore <$> arbitrary,
        minmax
      ]
    where
      minmax = do
        q <- arbitrary
        r <- arbitrary

        -- Special handling of the `\x{1,2}` case, which is an
        -- example of a Metacharacter which cannot be parsed by PCRE
        if q == bsZero
          then arbitrary :: Gen Metacharacter
          else return $ MinMax q r
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
