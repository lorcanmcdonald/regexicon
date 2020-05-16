{-# LANGUAGE OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE.Render where
import Data.List
import Test.QuickCheck.Regex.PCRE.Types

toText :: Regex -> String
toText (Regex chars) = concatMap render chars
toText (Alternative first second []) =
  let
    asRegex = toText . Regex
  in
  intercalate "|" [ asRegex first, asRegex second]
toText (Alternative first second rest) =
  let
    asRegex = toText . Regex
    xs = intercalate "|" $ map asRegex rest
  in
  intercalate "|" [ asRegex first, asRegex second, xs ]
toText (StartOfString re) = "^" <> concatMap render re
toText (EndOfString re) = concatMap render re <> "$"
toText (StartAndEndOfString re) = "^" <> concatMap render re <> "$"

class RegexRenderer a where
  render :: a -> String

instance RegexRenderer RegexCharacter where
  render (Quant q) = render q
  render (Meta m) = render m

instance RegexRenderer Quantifiable where
  render AnyCharacter = "."
  render (Backslash (Nonalphanumeric c)) = "\\" <> (c : "")
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
  render (CharacterClass chars) = "[" <> concatMap render chars <> "]"
  render (NegatedCharacterClass chars) = "[^" <> concatMap render chars <> "]"
  render (Subpattern chars) = "(" <> concatMap render chars <> ")"

instance RegexRenderer MetaCharacter where
  render (ZeroOrMore q) = render q <> "*"
  render (OneOrMore q) = render q <> "+"
  render (MinMax q range) = render q <> "{" <> show a <> "," <> show b <> "}"
    where
      (a, b) = extractPositiveRange range

instance RegexRenderer CharacterClassCharacter where
  render (ClassLiteral c) = [c]
  render (ClassRange r)
    =
      let (c, d) = extractRange r
      in [c] <> "-" <> [d]
