{-# LANGUAGE DeriveDataTypeable #-}

module Test.QuickCheck.Regex.PCRE.Types.CharacterClassCharacter
  ( CharacterClassCharacter (),
    ClassBackslashSequence (..),
    characterClassCharacter,
    characterClassCharacters,
    inCharacterClassCharacter,
  )
where

import Control.Lens.Combinators (_Left, over)
import Data.Data
import Data.Functor (($>))
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Text.ParserCombinators.Parsec

characterClassCharacter :: String -> Either String CharacterClassCharacter
characterClassCharacter = over _Left show . parse characterClassCharacters ""

data CharacterClassCharacter
  = ClassLiteral Char
  | ClassRange (OrderedRange Char)
  | QuotedClassLiterals Char String
  | ClassBackslash ClassBackslashSequence
  deriving (Data, Eq, Show)

instance Exemplify CharacterClassCharacter where
  examples (ClassLiteral c) = elements [[c]]
  examples (ClassRange r) =
    fmap (: "") . choose $ extractRange r
  examples (QuotedClassLiterals c s) = oneof $ pure . (: []) <$> (c : s)
  examples (ClassBackslash s) = examples s

instance Arbitrary CharacterClassCharacter where
  arbitrary =
    oneof
      [ ClassLiteral <$> regexChars,
        -- ClassRange <$> arbitrary, -- Takes a long time to calculate :-/ (probably not specialised for characters)

        -- QuotedClassLiterals do not seem to be parsed as I understand in
        -- PCRE3, avoiding generating them for now
        -- QuotedClassLiterals
        --   <$> arbitraryASCIIChar
        --     `suchThat` ( \c ->
        --                    ord c > 31
        --                )
        --   <*> (getASCIIString <$> (arbitrary :: Gen ASCIIString))
        --     `suchThat` ( \x ->
        --                    (not . null $ x)
        --                      && all (\c -> ord c > 31 && ord c /= 45) x
        --                ),
        ClassBackslash <$> arbitrary
      ]

  shrink (ClassLiteral _) = []
  shrink (ClassRange range) = ClassRange <$> shrink range
  shrink (QuotedClassLiterals _ []) = []
  shrink (QuotedClassLiterals c s) =
    QuotedClassLiterals <$> shrink c <*> shrink s
  shrink (ClassBackslash s) = ClassBackslash <$> shrink s

regexChars :: Gen Char
regexChars = oneof [choose ('a', 'z'), choose ('A', 'Z'), choose ('0', '9')] -- TODO Duplicate code, also Extend to non-metacharacter chars

inCharacterClassCharacter :: Char -> CharacterClassCharacter -> Bool
inCharacterClassCharacter c (ClassLiteral l) = c == l
inCharacterClassCharacter c (ClassRange r) =
  c >= a && c <= b
  where
    (a, b) = extractRange r
inCharacterClassCharacter c (QuotedClassLiterals s q) = c `elem` (s : q)
inCharacterClassCharacter c (ClassBackslash CCHyphen) = c == '-'
inCharacterClassCharacter c (ClassBackslash CCDigit) = c `elem` ("0123456789" :: String)

instance RegexRenderer CharacterClassCharacter where
  render (ClassLiteral c) = [c]
  render (ClassRange r) =
    let (c, d) = extractRange r
     in [c] <> "-" <> [d]
  render (QuotedClassLiterals c s) = "\\Q" <> [c] <> s <> "\\E"
  render (ClassBackslash s) = render s

characterClassCharacters :: GenParser Char st CharacterClassCharacter
characterClassCharacters =
  try
    ( do
        a <- validChars
        _ <- string "-"
        b <- validChars
        case orderedRange a b of
          Just range -> return $ ClassRange range
          Nothing -> fail "Range out of order in charcter class"
    )
    <|> try (ClassLiteral <$> validChars)
    <|> try
      ( QuotedClassLiterals <$> (string "\\Q" *> anyChar)
          <*> manyTill anyChar (try (string "\\E"))
      )
    <|> try classBackslashSequence
    <?> "CharacterClassCharacter"
  where
    validChars = noneOf "\\^[]-"

data ClassBackslashSequence
  = CCHyphen
  | CCDigit
  deriving (Data, Eq, Show)

instance Arbitrary ClassBackslashSequence where
  arbitrary =
    oneof
      [ pure CCHyphen,
        pure CCDigit
      ]
  shrink _ = []

instance Exemplify ClassBackslashSequence where
  examples CCHyphen = pure "-"
  examples CCDigit =
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

instance RegexRenderer ClassBackslashSequence where
  render CCHyphen = "\\-"
  render CCDigit = "\\d"

classBackslashSequence :: GenParser Char st CharacterClassCharacter
classBackslashSequence =
  ClassBackslash
    <$> ( string "\\"
            *> ( try (string "-" $> CCHyphen)
                   <|> try (string "d" $> CCDigit)
               )
        )
    <?> "classBackslashSequence"
