{-# LANGUAGE OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE.Render where
import Data.Monoid
import Test.QuickCheck.Regex.PCRE.Types

toText :: Regex -> String
toText (Regex chars) = concatMap render chars
toText (Alternative re1 re2) = toText re1 <> "|" <> toText re2
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
  render (Character c) = [c]
  render (CharacterClass chars) = "[" <> concatMap render chars <> "]"
  render (NegatedCharacterClass chars) = "[^" <> concatMap render chars <> "]"
  render (Subpattern re) = "(" <> toText re <> ")"

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
