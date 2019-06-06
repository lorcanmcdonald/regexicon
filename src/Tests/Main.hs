{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.Either.Extra (isLeft)
import Test.Framework
import Test.QuickCheck.Regex.PCRE

main :: IO ()
main = htfMain htf_thisModulesTests

test_empty :: IO ()
test_empty = assertEqual (Right (Regex [ ])) (parseRegex "")

test_single_char :: IO ()
test_single_char = assertEqual (Right (Regex [Quant (Character 'a')])) (parseRegex "a")

test_meta_char :: IO ()
test_meta_char = assertEqual (Right (Regex [Quant AnyCharacter])) (parseRegex ".")

test_multiple_char :: IO ()
test_multiple_char = assertEqual (Right (Regex [Quant (Character 'a'), Meta EndOfString])) (parseRegex "a$")

test_invalid_pattern :: IO ()
test_invalid_pattern = assertBool . isLeft . parseRegex $ "*"

test_alternatives :: IO ()
test_alternatives = assertEqual
  (Right
        (Alternative
          (Regex [Quant (Character 'a')])
          (Regex [Quant (Character 'b')])
        ))
  (parseRegex "a|b")

test_zero_or_more :: IO ()
test_zero_or_more = assertEqual
  (parseRegex "a*")
  (Right (Regex [Meta (ZeroOrMore (Character 'a'))]))

test_one_or_more :: IO ()
test_one_or_more = assertEqual
  (parseRegex "a+")
  (Right (Regex [Meta (OneOrMore (Character 'a'))]))

test_subpattern :: IO ()
test_subpattern = assertEqual
  (parseRegex "a(b)")
  (Right (Regex [ Quant (Character 'a'), Quant (Subpattern (Regex[ Quant (Character 'b')]))]))

test_character_class :: IO ()
test_character_class = assertEqual
  (parseRegex "[ab]")
  (Right (Regex [ Quant (CharacterClass [ ClassLiteral 'a', ClassLiteral 'b'])]))
