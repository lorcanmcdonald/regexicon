{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.QuickCheck.Regex.PCRE.Types.Metacharacters where

import Data.Data
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
-- import {-# SOURCE #-} Test.QuickCheck.Regex.PCRE.Types.Quantifiable
--   ( Quantifiable (),
--     backslashSequence,
--   )
import Test.QuickCheck.Regex.PCRE.Types.Quantifiable
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Text.ParserCombinators.Parsec

data Metacharacter
  = ZeroOrMore Quantifiable
  | ZeroOrOne Quantifiable
  | OneOrMore Quantifiable
  | MinMax Quantifiable (CountRange Int)
  deriving (Data, Eq, Generic, Show)

bsZero :: Quantifiable
bsZero = case parse backslashSequence "" "\\x" of
  Left s -> error ("Developer error: `bsZero` cannot be parsed.\n\t" <> show s)
  Right x -> x

instance Arbitrary Metacharacter where
  arbitrary =
    oneof
      [ ZeroOrMore <$> arbitrary,
        ZeroOrOne <$> arbitrary,
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

  shrink x = subterms x <> recursivelyShrink x

-- shrink (ZeroOrMore AnyCharacter) = []
-- shrink (ZeroOrMore q) = ZeroOrMore <$> shrink q
-- shrink (OneOrMore q) = [ZeroOrMore q] <> (OneOrMore <$> shrink q)
-- shrink (MinMax q range) =
--   [ZeroOrMore q]
--     <> (MinMax <$> shrink q <*> shrink range)

instance Exemplify Metacharacter where
  examples (ZeroOrMore q) = fmap concat . listOf $ examples q
  examples (ZeroOrOne _) = oneof [vector 0, vector 1]
  examples (OneOrMore q) = fmap concat . listOf1 $ examples q
  -- examples (Min q r) = _
  -- examples (Max q r) = _
  examples (MinMax q r) = do
    let (a, b) :: (Int, Int) = extractRange r
    k <- choose (a, b)
    fmap concat . vectorOf k $ examples q

instance RegexRenderer Metacharacter where
  render (ZeroOrMore q) = render q <> "*"
  render (ZeroOrOne q) = render q <> "?"
  render (OneOrMore q) = render q <> "+"
  render (MinMax q range) = render q <> "{" <> show a <> "," <> show b <> "}"
    where
      (a, b) :: (Int, Int) = extractRange range
