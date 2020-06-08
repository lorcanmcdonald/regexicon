{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Render where

import Data.List
import Numeric (showHex, showOct)
import Test.QuickCheck.Regex.PCRE.Types

toText :: Regex -> String
toText = render

class RegexRenderer a where
  render :: a -> String

instance RegexRenderer Regex where
  render (Regex chars) = render chars
  render (StartOfString re) = "^" <> render re
  render (EndOfString re) = render re <> "$"
  render (StartAndEndOfString re) = "^" <> render re <> "$"

instance RegexRenderer Pattern where
  render (Alternative x xs) = intercalate "|" $ map render x <> map render xs

instance RegexRenderer [RegexCharacter] where
  render chars = concatMap render chars

instance RegexRenderer RegexCharacter where
  render (Quant q) = render q
  render (Meta m) = render m
  render (Quoted s) = s

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
  render (Backslash CloseSquareBracket) = "\\["
  render (Backslash NonprintingAlarm) = "\\a"
  render (Backslash (NonprintingCtrlx x)) = "\\c" <> [x]
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
  render (BackReference n _) = "\\" <> (show n)
  render (Character c) = [c]
  render (CharacterClass firstChar chars) = "[" <> render firstChar <> concatMap render chars <> "]"
  render (NegatedCharacterClass firstChar chars) = "[^" <> render firstChar <> concatMap render chars <> "]"
  render (Subpattern re) = "(" <> render re <> ")"

instance RegexRenderer MetaCharacter where
  render (ZeroOrMore q) = render q <> "*"
  render (OneOrMore q) = render q <> "+"
  render (MinMax q range) = render q <> "{" <> show a <> "," <> show b <> "}"
    where
      (a, b) = extractPositiveRange range

instance RegexRenderer CharacterClassCharacter where
  render (ClassLiteral c) = [c]
  render (ClassRange r) =
    let (c, d) = extractRange r
     in [c] <> "-" <> [d]
  render (QuotedClassLiterals c) = "\\Q" <> c <> "\\E"
