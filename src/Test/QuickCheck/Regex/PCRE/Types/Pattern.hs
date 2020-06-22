{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Types.Pattern (Pattern (..)) where

import Data.Data
import Data.List
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Test.QuickCheck.Regex.PCRE.Types.RegexCharacter

data Pattern
  = Alternative [RegexCharacter] [[RegexCharacter]]
  deriving (Data, Eq, Show)

instance Arbitrary Pattern where
  arbitrary =
    oneof
      [Alternative <$> listOf1 arbitrary <*> arbitrary]

  shrink (Alternative [] []) = []
  shrink (Alternative c []) = (`Alternative` []) <$> shrink c
  shrink (Alternative x xs) =
    [Alternative x []]
      <> map (`Alternative` [])
        ( concat . shrink $
            (x : xs)
        )

instance Exemplify Pattern where
  examples (Alternative x xs) = oneof . map examples $ x : xs

instance RegexRenderer Pattern where
  render (Alternative x xs) = intercalate "|" $ map render x <> map render xs
