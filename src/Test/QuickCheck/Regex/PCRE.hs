{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE
  ( BackslashSequence (..),
    CharacterClassCharacter (..),
    MetaCharacter (..),
    OrderedRange,
    Quantifiable (..),
    Regex (..),
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
matching (Regex reChar) = charList reChar
matching (Alternative first second rest) = oneof . map charList $ (first : second : rest)
matching (StartOfString reChar) = charList reChar
matching (EndOfString reChar) = charList reChar
matching (StartAndEndOfString reChar) = charList reChar

charList :: [RegexCharacter] -> Gen String
charList = fmap concat . traverse matchingChar

matchingChar :: RegexCharacter -> Gen String
matchingChar (Quant q) = matchingQuantifiable q
matchingChar (Meta m) = matchingMeta m
matchingChar (Quoted s) = pure s

matchingQuantifiable :: Quantifiable -> Gen String
matchingQuantifiable AnyCharacter = fmap (: "") regexChars
matchingQuantifiable (Backslash b) = matchingBackslash b
matchingQuantifiable (Character char) = elements [[char]]
matchingQuantifiable (CharacterClass firstChar chars) =
  oneof $ matchingCharacterClassCharacters <$> (firstChar : chars)
matchingQuantifiable (NegatedCharacterClass firstChar chars) =
  (: []) <$> regexChars
    `suchThat` (\a -> (not . any (inCharacterClassCharacter a)) (firstChar : chars))
matchingQuantifiable (Subpattern re) = charList re

matchingMeta :: MetaCharacter -> Gen String
matchingMeta (ZeroOrMore q) = fmap concat . listOf $ matchingQuantifiable q
matchingMeta (OneOrMore q) = fmap concat . listOf1 $ matchingQuantifiable q
matchingMeta (MinMax q r) = do
  let (a, b) = extractPositiveRange r
  k <- choose (a, b)
  fmap concat . vectorOf k $ matchingQuantifiable q

matchingBackslash :: BackslashSequence -> Gen String
matchingBackslash (Nonalphanumeric c) = pure (c : "")
matchingBackslash BackslashChar = pure "\\"
matchingBackslash Caret = pure "\\^"
matchingBackslash Dollar = pure "\\$"
matchingBackslash Dot = pure "\\."
matchingBackslash OpenSquareBracket = pure "\\["
matchingBackslash Pipe = pure "\\|"
matchingBackslash OpenParens = pure "\\("
matchingBackslash CloseParens = pure "\\)"
matchingBackslash QuestionMark = pure "\\?"
matchingBackslash Asterisk = pure "\\*"
matchingBackslash Plus = pure "\\+"
matchingBackslash OpenBrace = pure "\\{"
matchingBackslash Hyphen = pure "\\-"
matchingBackslash CloseSquareBracket = pure "\\]"
matchingBackslash Digit =
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
matchingBackslash NonDigit =
  fmap (: "") (arbitraryASCIIChar `suchThat` (not . isDigit))
matchingBackslash HorizontalWhiteSpace =
  (: "") <$> oneof (map return horizontalSpace)
matchingBackslash NotHorizontalWhiteSpace =
  fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return horizontalSpace)
matchingBackslash VerticalWhiteSpace =
  (: "") <$> oneof (map return verticalSpace)
matchingBackslash NotVerticalWhiteSpace =
  fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return verticalSpace)
matchingBackslash WhiteSpace =
  (: "") <$> oneof (map return whiteSpace)
matchingBackslash NotWhiteSpace =
  fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return whiteSpace)
matchingBackslash WordCharacter =
  (: "") <$> oneof (map return wordChar)
matchingBackslash NonWordCharacter =
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

matchingCharacterClassCharacters :: CharacterClassCharacter -> Gen String
matchingCharacterClassCharacters (ClassLiteral c) = elements [[c]]
matchingCharacterClassCharacters (ClassRange r) =
  fmap (: "") . choose $ extractRange r
