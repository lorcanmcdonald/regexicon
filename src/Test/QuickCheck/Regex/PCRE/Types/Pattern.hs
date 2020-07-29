{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Types.Pattern (Pattern (..), mkAlternative) where

import Data.Data
import Data.List
import Data.List.NonEmpty (NonEmpty, fromList, toList)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe
import Test.QuickCheck
import qualified Test.QuickCheck.Modifiers as Modifiers
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Test.QuickCheck.Regex.PCRE.Types.RegexCharacter

newtype Pattern
  = Alternative (NonEmpty RegexCharacterList)
  deriving (Data, Eq, Show)

genNonempty :: Arbitrary a => Gen (NonEmpty a)
genNonempty = NonEmpty.fromList . Modifiers.getNonEmpty <$> arbitrary

shrinkNonempty :: Arbitrary a => NonEmpty a -> [NonEmpty a]
shrinkNonempty a =
  NonEmpty.fromList . Modifiers.getNonEmpty
    <$> (shrink . Modifiers.NonEmpty . NonEmpty.toList $ a)

instance Arbitrary Pattern where
  arbitrary =
    oneof [Alternative <$> genNonempty]

  shrink (Alternative xs) =
    toList (Alternative . fromList . (: []) <$> xs)
      <> catMaybes (mkAlternative <$> shrinkNonempty xs)

mkAlternative :: NonEmpty RegexCharacterList -> Maybe Pattern
mkAlternative xs = do
  (RegexCharacterList res) <- foldr badEmpties (Just (RegexCharacterList [])) xs
  if null res
    then Just . Alternative $ xs
    else Nothing
  where
    badEmpties ::
      RegexCharacterList ->
      Maybe RegexCharacterList ->
      Maybe RegexCharacterList
    badEmpties (RegexCharacterList []) (Just (RegexCharacterList [])) = Nothing
    badEmpties next _ = Just next

instance Exemplify Pattern where
  examples (Alternative xs) = oneof . toList $ examples <$> xs

instance RegexRenderer Pattern where
  render (Alternative xs) = intercalate "|" . toList $ render <$> xs
