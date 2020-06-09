{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE
  ( BackslashSequence (..),
    CharacterClassCharacter (..),
    MetaCharacter (..),
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

import Data.Char (isDigit)
import Numeric (showHex, showOct)
import Test.QuickCheck
  ( Gen,
    arbitraryASCIIChar,
    choose,
    elements,
    listOf,
    listOf1,
    oneof,
    suchThat,
    vectorOf,
  )
import Test.QuickCheck.Regex.PCRE.Parse
import Test.QuickCheck.Regex.PCRE.Render
import Test.QuickCheck.Regex.PCRE.Types

matching :: Regex -> Gen String
matching = examples

class Exemplify a where
  examples :: a -> Gen String

instance Exemplify Regex where
  examples (Regex reChar) = examples reChar
  examples (StartOfString reChar) = examples reChar
  examples (EndOfString reChar) = examples reChar
  examples (StartAndEndOfString reChar) = examples reChar

instance Exemplify Pattern where
  examples (Alternative x xs) = oneof . map examples $ x : xs

instance Exemplify [RegexCharacter] where
  examples = fmap concat . traverse examples

instance Exemplify RegexCharacter where
  examples (Quant q) = examples q
  examples (Meta m) = examples m
  examples (Quoted s) = pure s

instance Exemplify Quantifiable where
  examples AnyCharacter = fmap (: "") regexChars
  examples (AmbiguousNumberSequence _) = error "Should not generate an AmbiguousNumberSequence"
  examples (Backslash b) = examples b
  examples (BackReference _ p) = examples p
  examples (Character char) = elements [[char]]
  examples (CharacterClass firstChar chars) =
    oneof $ examples <$> (firstChar : chars)
  examples (NegatedCharacterClass firstChar chars) =
    (: []) <$> regexChars
      `suchThat` (\a -> (not . any (inCharacterClassCharacter a)) (firstChar : chars))
  examples (Subpattern re) = examples re

instance Exemplify MetaCharacter where
  examples (ZeroOrMore q) = fmap concat . listOf $ examples q
  examples (OneOrMore q) = fmap concat . listOf1 $ examples q
  examples (MinMax q r) = do
    let (a, b) = extractPositiveRange r
    k <- choose (a, b)
    fmap concat . vectorOf k $ examples q

instance Exemplify BackslashSequence where
  examples (Nonalphanumeric c) = pure (c : "")
  examples BackslashChar = pure "\\"
  examples Caret = pure "\\^"
  examples Dollar = pure "\\$"
  examples Dot = pure "\\."
  examples OpenSquareBracket = pure "\\["
  examples Pipe = pure "\\|"
  examples OpenParens = pure "\\("
  examples CloseParens = pure "\\)"
  examples QuestionMark = pure "\\?"
  examples Asterisk = pure "\\*"
  examples Plus = pure "\\+"
  examples OpenBrace = pure "\\{"
  examples Hyphen = pure "\\-"
  examples CloseSquareBracket = pure "\\]"
  examples NonprintingAlarm = pure "\\a"
  examples (NonprintingCtrlx c) = pure ("\\c" <> [c])
  examples NonprintingEscape = pure "\\e"
  examples NonprintingFormFeed = pure "\\f"
  examples NonprintingLineFeed = pure "\\n"
  examples NonprintingCarriageReturn = pure "\\r"
  examples NonprintingTab = pure "\\t"
  examples (NonprintingOctalCodeZero c) = pure ("\\0" <> showOct c "")
  examples (NonprintingOctalCode c) = pure ("\\" <> showOct c "")
  examples (NonprintingOctalCodeBraces c) = pure ("\\o{" <> showOct c "" <> "}")
  examples NonprintingHexZero = pure "\\x"
  examples (NonprintingHexCode c) = pure ("\\x" <> showHex c "")
  examples (NonprintingHexCodeBraces c) = pure ("\\x{" <> showHex c "" <> "}")
  examples Digit =
    oneof
      [ pure "0",
        pure "1",
        pure "2",
        pure "3",
        pure "4",
        pure "5",
        pure "6",
        pure "7",
        pure "8",
        pure "9"
      ]
  examples NonDigit =
    fmap (: "") (arbitraryASCIIChar `suchThat` (not . isDigit))
  examples HorizontalWhiteSpace =
    (: "") <$> oneof (map return horizontalSpace)
  examples NotHorizontalWhiteSpace =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return horizontalSpace)
  examples VerticalWhiteSpace =
    (: "") <$> oneof (map return verticalSpace)
  examples NotVerticalWhiteSpace =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return verticalSpace)
  examples WhiteSpace =
    (: "") <$> oneof (map return whiteSpace)
  examples NotWhiteSpace =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return whiteSpace)
  examples WordCharacter =
    (: "") <$> oneof (map return wordChar)
  examples NonWordCharacter =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return wordChar)

horizontalSpace :: String
horizontalSpace =
  [ '\x0009', -- Horizontal tab (HT)
    '\x0020', -- Space
    '\x00A0', -- Non-break space
    '\x1680', -- Ogham space mark
    '\x180E', -- Mongolian vowel separator
    '\x2000', -- En quad
    '\x2001', -- Em quad
    '\x2002', -- En space
    '\x2003', -- Em space
    '\x2004', -- Three-per-em space
    '\x2005', -- Four-per-em space
    '\x2006', -- Six-per-em space
    '\x2007', -- Figure space
    '\x2008', -- Punctuation space
    '\x2009', -- Thin space
    '\x200A', -- Hair space
    '\x202F', -- Narrow no-break space
    '\x205F', -- Medium mathematical space
    '\x3000' -- Ideographic space
  ]

verticalSpace :: String
verticalSpace =
  [ '\x000A', -- Linefeed (LF)
    '\x000B', -- Vertical tab (VT)
    '\x000C', -- Form feed (FF)
    '\x000D', -- Carriage return (CR)
    '\x0085', -- Next line (NEL)
    '\x2028', -- Line separator
    '\x2029' -- Paragraph separator
  ]

whiteSpace :: String
whiteSpace = horizontalSpace <> verticalSpace

wordChar :: String
wordChar = ['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z']

instance Exemplify CharacterClassCharacter where
  examples (ClassLiteral c) = elements [[c]]
  examples (ClassRange r) =
    fmap (: "") . choose $ extractRange r
  examples (QuotedClassLiterals c) = elements [c]
