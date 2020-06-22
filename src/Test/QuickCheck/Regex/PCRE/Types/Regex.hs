{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickCheck.Regex.PCRE.Types.Regex where

import Control.Lens.Plated
import Data.Data
import Data.Data.Lens
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.Types.Pattern
import Test.QuickCheck.Regex.PCRE.RegexRenderer

data Regex
  = Regex Pattern
  | StartOfString Pattern
  | EndOfString Pattern
  | StartAndEndOfString Pattern
  deriving (Data, Eq, Generic, Show)

instance Arbitrary Regex where
  arbitrary =
    oneof
      [ Regex <$> arbitrary,
        StartOfString <$> arbitrary,
        EndOfString <$> arbitrary,
        StartAndEndOfString <$> arbitrary
      ]
  shrink = genericShrink

instance Plated Regex where
  plate = uniplate

instance Exemplify Regex where
  examples (Regex reChar) = examples reChar
  examples (StartOfString reChar) = examples reChar
  examples (EndOfString reChar) = examples reChar
  examples (StartAndEndOfString reChar) = examples reChar

instance RegexRenderer Regex where
  render (Regex chars) = render chars
  render (StartOfString re) = "^" <> render re
  render (EndOfString re) = render re <> "$"
  render (StartAndEndOfString re) = "^" <> render re <> "$"
