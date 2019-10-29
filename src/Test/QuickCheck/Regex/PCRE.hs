{-# LANGUAGE OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE
  ( Regex (..)
  , RegexCharacter (..)
  , MetaCharacter (..)
  , Quantifiable (..)
  , CharacterClassCharacter (..)
  , OrderedRange
  , matching
  , parseRegex
  , toText
  , extractRange
  ) where
import Test.QuickCheck
import Test.QuickCheck.Regex.PCRE.Parse
import Test.QuickCheck.Regex.PCRE.Render
import Test.QuickCheck.Regex.PCRE.Types

matching :: Regex -> Gen String
matching (Regex reChar) = charList reChar
matching (Alternative re1 re2) = oneof [matching re1, matching re2]
matching (StartOfString reChar) = charList reChar
matching (EndOfString reChar) = charList reChar
matching (StartAndEndOfString reChar) = charList reChar

charList :: [RegexCharacter] -> Gen String
charList = fmap concat . sequence . fmap matchingChar

matchingChar :: RegexCharacter -> Gen String
matchingChar (Quant q) = matchingQuantifiable q
matchingChar (Meta m) = matchingMeta m

matchingQuantifiable :: Quantifiable -> Gen String
matchingQuantifiable AnyCharacter = fmap (: "") regexChars
matchingQuantifiable (Character char) = elements [[char]]
matchingQuantifiable (CharacterClass chars)
  = oneof $ matchingCharacterClassCharacters <$> chars
matchingQuantifiable (NegatedCharacterClass chars)
  = (: []) <$> regexChars
  `suchThat` (\ a -> all (not . inCharacterClassCharacter a) chars)
matchingQuantifiable (Subpattern re) = matching re

matchingMeta :: MetaCharacter -> Gen String
matchingMeta (ZeroOrMore q) = fmap concat . listOf $ matchingQuantifiable q
matchingMeta (OneOrMore q) = fmap concat . listOf1 $ matchingQuantifiable q
matchingMeta (MinMax q r) = do
  let (a, b) = extractPositiveRange r
  k <- choose (a, b)
  fmap concat . vectorOf k $ matchingQuantifiable q

matchingCharacterClassCharacters :: CharacterClassCharacter -> Gen String
matchingCharacterClassCharacters (ClassLiteral c) = elements [[c]]
matchingCharacterClassCharacters (ClassRange r)
  = fmap (: "") . choose $ extractRange r
