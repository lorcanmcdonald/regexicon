module Test.QuickCheck.Regex.PCRE.RegexRenderer where

class RegexRenderer a where
  render :: a -> String

instance RegexRenderer Char where
  render c = [c]
