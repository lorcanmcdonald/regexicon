{-# LANGUAGE FlexibleInstances #-}

module Test.QuickCheck.Regex.PCRE
  ( BackslashSequence (..),
    CharacterClassCharacter,
    Metacharacter (..),
    OrderedRange,
    Quantifiable (..),
    Regex (..),
    Pattern (..),
    RegexCharacter (..),
    extractRange,
    matching,
    orderedRange,
    parseRegex,
    positiveOrderedRange,
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
