{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.QuickCheck.Regex.PCRE.Types.RegexCharacter (RegexCharacter (..), RegexCharacterList (..)) where

import Data.Data
import GHC.Generics
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Test.QuickCheck.Regex.PCRE.Types.Metacharacters
import {-# SOURCE #-} Test.QuickCheck.Regex.PCRE.Types.Quantifiable

data RegexCharacter
  = Quant Quantifiable
  | Meta Metacharacter
  | Quoted String
  deriving (Data, Eq, Generic, Show)

instance Arbitrary RegexCharacter where
  arbitrary =
    oneof
      [ Quant <$> arbitrary,
        Meta <$> arbitrary
      ]
  shrink = genericShrink

instance Exemplify RegexCharacter where
  examples (Quant q) = examples q
  examples (Meta m) = examples m
  examples (Quoted s) = pure s

instance RegexRenderer RegexCharacter where
  render (Quant q) = render q
  render (Meta m) = render m
  render (Quoted s) = s

newtype RegexCharacterList = RegexCharacterList [RegexCharacter]
  deriving (Data, Eq, Generic, Show)

instance Arbitrary RegexCharacterList where
  arbitrary = arbitrary
  shrink = genericShrink

instance Exemplify RegexCharacterList where
  examples (RegexCharacterList chars) = fmap concat . traverse examples $ chars

instance RegexRenderer RegexCharacterList where
  render (RegexCharacterList chars) = concatMap render chars
