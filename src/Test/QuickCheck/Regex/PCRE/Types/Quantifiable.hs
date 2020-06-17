{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Types.Quantifiable
  ( Quantifiable (..),
    backslashSequence,
  )
where

import Control.Lens.Plated
import Data.Char
import Data.Data
import Data.Data.Lens
import Data.Functor (($>))
import Numeric (readHex, readOct, showHex, showOct)
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Test.QuickCheck.Regex.PCRE.Types.Backslashes
import Test.QuickCheck.Regex.PCRE.Types.CharacterClassCharacter
import {-# SOURCE #-} Test.QuickCheck.Regex.PCRE.Types.Pattern
import Text.ParserCombinators.Parsec

data Quantifiable
  = AnyCharacter
  | Character Char
  | AmbiguousNumberSequence String
  | Backslash BackslashSequence
  | BackReference Int Pattern
  | CharacterClass CharacterClassCharacter [CharacterClassCharacter]
  | NegatedCharacterClass CharacterClassCharacter [CharacterClassCharacter]
  | Subpattern Pattern
  deriving (Data, Eq, Show)

instance Plated Quantifiable where
  plate = uniplate

regexChars :: Gen Char
regexChars = oneof [choose ('a', 'z'), choose ('A', 'Z'), choose ('0', '9')] -- TODO Extend to non-metacharacter chars

instance Arbitrary Quantifiable where
  arbitrary = sized quant'
    where
      quant' n
        | n > 3 =
          oneof
            [ pure AnyCharacter,
              Character <$> regexChars,
              Backslash <$> arbitrary,
              CharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary), -- CharacterClass must have at least one element
              NegatedCharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary) -- NegatedCharacterClass must have at least one element
            ]
      quant' n
        | n >= 0 && n <= 3 -- Subpattern can cause very deep trees at larger sizes
          =
          oneof
            [ pure AnyCharacter,
              Character <$> regexChars,
              Backslash <$> arbitrary,
              CharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary), -- CharacterClass must have at least one element
              NegatedCharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary), -- NegatedCharacterClass must have at least one element
              Subpattern <$> arbitrary
            ]
      quant' _ = pure AnyCharacter

  shrink AnyCharacter = [AnyCharacter]
  shrink (Character c) = Character <$> ['a', '\11', c]
  shrink self@(AmbiguousNumberSequence _) = [self]
  shrink (Backslash s) = Backslash <$> shrink s
  shrink (CharacterClass _ []) = []
  shrink (CharacterClass x xs) =
    [CharacterClass x []]
      <> ((`CharacterClass` []) <$> shrink x)
      <> map (`CharacterClass` []) xs
  shrink (NegatedCharacterClass _ []) = []
  shrink (NegatedCharacterClass x xs) =
    [NegatedCharacterClass x []]
      <> ((`NegatedCharacterClass` []) <$> shrink x)
      <> map (`NegatedCharacterClass` []) xs
  shrink self@(BackReference _ _) = [self]
  shrink (Subpattern p) = Subpattern <$> shrink p

instance Exemplify Quantifiable where
  examples AnyCharacter = fmap (: "") regexChars
  examples (AmbiguousNumberSequence _) = error "Should not generate an AmbiguousNumberSequence"
  examples (Backslash b) = examples b
  examples (BackReference _ p) = examples p
  examples (Character c) = elements [[c]]
  examples (CharacterClass firstChar chars) =
    oneof $ examples <$> (firstChar : chars)
  examples (NegatedCharacterClass firstChar chars) =
    (: []) <$> regexChars
      `suchThat` (\a -> (not . any (inCharacterClassCharacter a)) (firstChar : chars))
  examples (Subpattern re) = examples re

backslashSequence :: GenParser Char st Quantifiable
backslashSequence =
  Backslash
    <$> ( string "\\"
            *> ( try (string "^" $> Caret)
                   <|> try (string "$" $> Dollar)
                   <|> try (string "." $> Dot)
                   <|> try (string "[" $> OpenSquareBracket)
                   <|> try (string "|" $> Pipe)
                   <|> try (string "(" $> OpenParens)
                   <|> try (string ")" $> CloseParens)
                   <|> try (string "?" $> QuestionMark)
                   <|> try (string "*" $> Asterisk)
                   <|> try (string "+" $> Plus)
                   <|> try (string "{" $> OpenBrace)
                   <|> try (string "-" $> Hyphen)
                   <|> try (string "]" $> CloseSquareBracket)
                   <|> try (string "a" $> NonprintingAlarm)
                   -- <|> try (NonprintingCtrlx <$> (string "c" *> anyChar))
                   <|> try (string "e" $> NonprintingEscape)
                   <|> try (string "f" $> NonprintingFormFeed)
                   <|> try (string "n" $> NonprintingLineFeed)
                   <|> try (string "r" $> NonprintingCarriageReturn)
                   <|> try (string "t" $> NonprintingTab)
                   <|> try
                     ( do
                         octText <-
                           try (count 3 octDigit)
                             <|> try (count 2 octDigit)
                             <|> try (string "0") -- PCRE spec implies that \8 and \9 could be interpreted as octals, but `perl` does not
                         let (octValue, _) = head . readOct $ octText
                         return . NonprintingOctalCode $ octValue
                     )
                   <|> try
                     ( do
                         _ <- string "o{"
                         octText <- many1 octDigit
                         let (octValue, _) = head . readOct $ octText
                         _ <- string "}"
                         return . NonprintingOctalCodeBraces $ octValue
                     )
                   <|> try
                     ( do
                         _ <- string "x{"
                         hexText <- many1 hexDigit
                         let (hexValue, _) = head . readHex $ hexText
                         _ <- string "}"
                         return . NonprintingHexCodeBraces $ hexValue
                     )
                   <|> try
                     ( do
                         _ <- string "x"
                         hexText <-
                           try (count 2 hexDigit)
                             <|> try (count 1 hexDigit)
                         let hexParse = readHex hexText
                         return . NonprintingHexCode $
                           case hexParse of
                             [] -> 0 :: Int
                             (val, _) : _ -> val
                     )
                   -- <|> try (string "x" $> NonprintingHexZero)
                   <|> try
                     ( do
                         _ <- string "x{"
                         hexText <-
                           try (count 2 hexDigit)
                             <|> try (count 1 hexDigit)
                         _ <- string "}"
                         let (hexValue, _) = head . readHex $ hexText
                         return . NonprintingHexCodeBraces $ hexValue
                     )
                   <|> try (string "d" $> Digit)
                   <|> try (string "D" $> NonDigit)
                   <|> try (string "h" $> HorizontalWhiteSpace)
                   <|> try (string "H" $> NotHorizontalWhiteSpace)
                   <|> try (string "s" $> WhiteSpace)
                   <|> try (string "S" $> NotWhiteSpace)
                   <|> try (string "v" $> VerticalWhiteSpace)
                   <|> try (string "V" $> NotVerticalWhiteSpace)
                   <|> try (string "w" $> WordCharacter)
                   <|> try (string "W" $> NonWordCharacter)
                   <|> try (string "\\" $> BackslashChar)
                   <|> try (Nonalphanumeric <$> satisfy (not . isAlphaNum))
               )
        )

instance RegexRenderer Quantifiable where
  render AnyCharacter = "."
  render (AmbiguousNumberSequence s) = "\\" <> s
  render (Backslash (Nonalphanumeric c)) = "\\" <> (c : "")
  render (Backslash BackslashChar) = "\\\\"
  render (Backslash Caret) = "\\^"
  render (Backslash Dollar) = "\\$"
  render (Backslash Dot) = "\\."
  render (Backslash OpenSquareBracket) = "\\["
  render (Backslash Pipe) = "\\|"
  render (Backslash OpenParens) = "\\("
  render (Backslash CloseParens) = "\\)"
  render (Backslash QuestionMark) = "\\?"
  render (Backslash Asterisk) = "\\*"
  render (Backslash Plus) = "\\+"
  render (Backslash OpenBrace) = "\\{"
  render (Backslash Hyphen) = "\\-"
  render (Backslash CloseSquareBracket) = "\\]"
  render (Backslash NonprintingAlarm) = "\\a"
  -- render (Backslash (NonprintingCtrlx x)) = "\\c" <> [x]
  render (Backslash NonprintingEscape) = "\\e"
  render (Backslash NonprintingFormFeed) = "\\f"
  render (Backslash NonprintingLineFeed) = "\\n"
  render (Backslash NonprintingCarriageReturn) = "\\r"
  render (Backslash NonprintingTab) = "\\t"
  render (Backslash (NonprintingOctalCodeZero c)) = "\\0" <> showOct c ""
  render (Backslash (NonprintingOctalCode c)) = "\\" <> showOct c ""
  render (Backslash (NonprintingOctalCodeBraces c)) = "\\o{" <> showOct c "" <> "}"
  render (Backslash NonprintingHexZero) = "\\x"
  render (Backslash (NonprintingHexCode c)) = "\\x" <> showHex c ""
  render (Backslash (NonprintingHexCodeBraces c)) = "\\x{" <> showHex c "" <> "}"
  render (Backslash Digit) = "\\d"
  render (Backslash NonDigit) = "\\D"
  render (Backslash HorizontalWhiteSpace) = "\\h"
  render (Backslash NotHorizontalWhiteSpace) = "\\H"
  render (Backslash WhiteSpace) = "\\s"
  render (Backslash NotWhiteSpace) = "\\S"
  render (Backslash VerticalWhiteSpace) = "\\h"
  render (Backslash NotVerticalWhiteSpace) = "\\H"
  render (Backslash WordCharacter) = "\\w"
  render (Backslash NonWordCharacter) = "\\W"
  render (BackReference n _) = "\\" <> show n
  render (Character c) = [c]
  render (CharacterClass firstChar chars) = "[" <> render firstChar <> concatMap render chars <> "]"
  render (NegatedCharacterClass firstChar chars) = "[^" <> render firstChar <> concatMap render chars <> "]"
  render (Subpattern re) = "(" <> render re <> ")"
