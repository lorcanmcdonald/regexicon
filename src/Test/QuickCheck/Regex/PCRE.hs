{-# LANGUAGE FlexibleInstances #-}

module Test.QuickCheck.Regex.PCRE
  ( BackslashSequence (..),
    CharacterClassCharacter,
    Metacharacter (..),
    Quantifiable (..),
    Regex (..),
    Pattern (..),
    RegexCharacter (..),
    RegexCharacterList (..),
    extractRange,
    matching,
    parseRegex,
    toText,
  )
where

import Test.QuickCheck (Gen)
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.Parse
import Test.QuickCheck.Regex.PCRE.Render
import Test.QuickCheck.Regex.PCRE.Types

matching :: Regex -> Gen String
matching = examples
