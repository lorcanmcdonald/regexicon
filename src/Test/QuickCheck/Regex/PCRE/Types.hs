{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Types
  ( CharacterClassCharacter,
    BackslashSequence (..),
    ClassBackslashSequence (..),
    Metacharacter (..),
    Pattern (..),
    Quantifiable (..),
    Regex (..),
    RegexCharacter (..),
    RegexCharacterList (..),
    SubpatternContainer (..),
    backslashSequence,
    characterClassCharacter,
    characterClassCharacters,
    extractRange,
    inCharacterClassCharacter,
    nonalphanumeric,
  )
where

import Control.Lens ((^?), _Left, element, over)
import Control.Monad
import Test.QuickCheck.Regex.PCRE.Types.Backslashes
import Test.QuickCheck.Regex.PCRE.Types.CharacterClassCharacter
import Test.QuickCheck.Regex.PCRE.Types.Metacharacters
import Test.QuickCheck.Regex.PCRE.Types.Pattern
import Test.QuickCheck.Regex.PCRE.Types.Quantifiable
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Test.QuickCheck.Regex.PCRE.Types.Regex
import Test.QuickCheck.Regex.PCRE.Types.RegexCharacter
import Text.ParserCombinators.Parsec
import Text.Read

class SubpatternContainer a where
  numSubpatterns :: a -> Int
  numSubpatterns = length . subpatterns

  subpatterns :: a -> [Quantifiable]
  resolveBackreferences :: [Quantifiable] -> a -> Either String a

instance SubpatternContainer Regex where
  subpatterns (Regex reChars) = subpatterns reChars
  subpatterns (StartOfString reChars) = subpatterns reChars
  subpatterns (EndOfString reChars) = subpatterns reChars
  subpatterns (StartAndEndOfString reChars) = subpatterns reChars

  resolveBackreferences re (Regex reChars) =
    Regex <$> resolveBackreferences re reChars
  resolveBackreferences re (StartOfString reChars) =
    StartOfString <$> resolveBackreferences re reChars
  resolveBackreferences re (EndOfString reChars) =
    EndOfString <$> resolveBackreferences re reChars
  resolveBackreferences re (StartAndEndOfString reChars) =
    StartAndEndOfString <$> resolveBackreferences re reChars

instance SubpatternContainer Metacharacter where
  subpatterns (ZeroOrMore q) = subpatterns q
  subpatterns (ZeroOrOne q) = subpatterns q
  subpatterns (OneOrMore q) = subpatterns q
  subpatterns (MinMax q _) = subpatterns q

  resolveBackreferences re (ZeroOrMore q) = ZeroOrMore <$> resolveBackreferences re q
  resolveBackreferences re (ZeroOrOne q) = ZeroOrOne <$> resolveBackreferences re q
  resolveBackreferences re (OneOrMore q) = OneOrMore <$> resolveBackreferences re q
  resolveBackreferences re (MinMax q r) = MinMax <$> resolveBackreferences re q <*> pure r

instance SubpatternContainer Pattern where
  subpatterns (Alternative xs) = concatMap subpatterns xs
  resolveBackreferences re (Alternative xs) = do
    xs' <- mapM (resolveBackreferences re) xs
    return $ Alternative xs'

instance SubpatternContainer Quantifiable where
  subpatterns AnyCharacter = []
  subpatterns (Character _) = []
  subpatterns (AmbiguousNumberSequence _) = []
  subpatterns (Backslash _) = []
  subpatterns (BackReference _ _) = []
  subpatterns (CharacterClass _) = []
  subpatterns (NegatedCharacterClass _) = []
  subpatterns self@(Subpattern reChars) = self : subpatterns reChars

  resolveBackreferences _ self@AnyCharacter = pure self
  resolveBackreferences _ self@(Character _) = pure self
  resolveBackreferences _ self@(Backslash _) = pure self
  resolveBackreferences _ self@(BackReference _ _) = pure self
  resolveBackreferences _ self@(CharacterClass _) = pure self
  resolveBackreferences _ self@(NegatedCharacterClass _) = pure self
  resolveBackreferences re (Subpattern reChars) = Subpattern <$> resolveBackreferences re reChars
  resolveBackreferences patterns (AmbiguousNumberSequence str) =
    case maybeBackref str of
      Nothing -> asBackslash str
      Just a -> Right a
    where
      maybeBackref :: String -> Maybe Quantifiable
      maybeBackref backRefStr = do
        -- traceShowM patterns
        patternIndex <- readMaybe backRefStr
        subpattern <- patterns ^? element (patternIndex - 1)
        case subpattern of
          Subpattern s -> return $ BackReference patternIndex s
          _ -> mzero
      asBackslash :: String -> Either String Quantifiable
      asBackslash backlashStr =
        over _Left show
          . parse backslashSequence ""
          $ ("\\" <> backlashStr)

instance SubpatternContainer RegexCharacterList where
  subpatterns (RegexCharacterList chars) =
    concatMap
      subpatterns
      chars

  resolveBackreferences re (RegexCharacterList chars) = do
    list <- mapM (resolveBackreferences re) chars
    return . RegexCharacterList $ list

instance SubpatternContainer RegexCharacter where
  subpatterns (Quant q) = subpatterns q
  subpatterns (Meta m) = subpatterns m
  subpatterns (Quoted _) = []

  resolveBackreferences re (Quant q) = Quant <$> resolveBackreferences re q
  resolveBackreferences re (Meta m) = Meta <$> resolveBackreferences re m
  resolveBackreferences _ self@(Quoted _) = Right self
