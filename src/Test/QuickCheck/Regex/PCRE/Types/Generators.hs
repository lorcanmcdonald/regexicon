module Test.QuickCheck.Regex.PCRE.Types.Generators
  ( regexChars,
  )
where

import Test.QuickCheck

-- Generates a printable ASCII character that is not a PCRE metacharacter.
-- PCRE metacharacters outside character classes: \ ^ $ . [ | ( ) * + ? {
regexChars :: Gen Char
regexChars = arbitraryPrintableChar `suchThat` (`notElem` "\\^$.[|()*+?{")
