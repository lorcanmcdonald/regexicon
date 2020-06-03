{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Render where

import Data.List
import Numeric (showHex, showOct)
import Test.QuickCheck.Regex.PCRE.Types

toText :: Regex -> String
toText (Regex chars) = concatMap render chars
toText (Alternative first second []) =
  let asRegex = toText . Regex
   in intercalate "|" [asRegex first, asRegex second]
toText (Alternative first second rest) =
  let asRegex = toText . Regex
      xs = intercalate "|" $ map asRegex rest
   in intercalate "|" [asRegex first, asRegex second, xs]
toText (StartOfString re) = "^" <> concatMap render re
toText (EndOfString re) = concatMap render re <> "$"
toText (StartAndEndOfString re) = "^" <> concatMap render re <> "$"

class RegexRenderer a where
  render :: a -> String

instance RegexRenderer RegexCharacter where
  render (Quant q) = render q
  render (Meta m) = render m
  render (Quoted s) = s

instance RegexRenderer Quantifiable where
  render AnyCharacter = "."
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
  render (Character c) = [c]
  render (CharacterClass firstChar chars) = "[" <> render firstChar <> concatMap render chars <> "]"
  render (NegatedCharacterClass firstChar chars) = "[^" <> render firstChar <> concatMap render chars <> "]"
  render (Subpattern chars) = "(" <> concatMap render chars <> ")"

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
