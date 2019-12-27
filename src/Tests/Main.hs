{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad
import Data.Either.Extra (isLeft)
import Data.List (intercalate)
import Data.Maybe
import Data.String.Conv
import System.Console.ANSI
import System.Environment
import System.IO (hFlush, stdout)
import Test.QuickCheck.Regex.PCRE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Regex.PCRE hiding (Regex)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Parse"
    [ testGroup "Simple patterns"
      [ testCase "a|b" test_alternatives
      , testCase "[ab]" test_character_class
      , testCase "[^a]" test_negated_character_class
      , testCase "\\*" test_escape_metachar
      , testCase "." test_meta_char
      , testCase "^a" test_multiple_char
      , testCase "a$" test_match_end
      , testCase "a+" test_one_or_more
      , testCase "a" test_single_char
      , testCase "a(b)" test_subpattern
      , testCase "a*" test_zero_or_more
      , testCase "[0-9a-f]{32}" test_website_example
      , testCase "\\d" test_digit
      ]

    , testGroup "Failure cases"
      [ testCase "empty string" test_empty
      , testCase "*" test_invalid_pattern
      ]

    , testGroup "Transitive properties"
      [ testCase "parse with PCRE lib" test_matching_produces_valid_matches
      , testCase "transitive example" test_transitive_a
      ]
    ]
  , testGroup "Matching"
    [ testCase "\\d" test_matching_digit
    ]
  ]

test_empty :: Assertion
test_empty = assertBool "Empty pattern doesn't fail" . isLeft . parseRegex $ ""

test_single_char :: Assertion
test_single_char =
  assertEqual "Could not parse pattern" (Right (Regex [Quant (Character 'a')])) (parseRegex "a")

test_meta_char :: Assertion
test_meta_char =
  assertEqual "Could not parse pattern" (Right (Regex [Quant AnyCharacter])) (parseRegex ".")

test_multiple_char :: Assertion
test_multiple_char =
  assertEqual "Could not parse pattern" (Right (StartOfString [Quant (Character 'a')])) (parseRegex "^a")

test_match_end :: Assertion
test_match_end =
  assertEqual "Could not parse pattern"
    (Right (EndOfString [Quant (Character 'a')])) (parseRegex "a$")

test_invalid_pattern :: Assertion
test_invalid_pattern = assertBool "Parsed an invalid pattern" . isLeft . parseRegex $ "*"

test_alternatives :: Assertion
test_alternatives = assertEqual "Could not parse pattern"
  (Right
        (Alternative
          (Regex [Quant (Character 'a')])
          (Regex [Quant (Character 'b')])
        ))
  (parseRegex "a|b")

test_zero_or_more :: Assertion
test_zero_or_more = assertEqual "Could not parse pattern"
  (parseRegex "a*")
  (Right (Regex [Meta (ZeroOrMore (Character 'a'))]))

test_one_or_more :: Assertion
test_one_or_more = assertEqual "Could not parse pattern"
  (parseRegex "a+")
  (Right (Regex [Meta (OneOrMore (Character 'a'))]))

test_subpattern :: Assertion
test_subpattern = assertEqual "Could not parse pattern"
  (parseRegex "a(b)")
  (Right
    (Regex [ Quant (Character 'a'), Quant (Subpattern (Regex [ Quant (Character 'b')]))]))

test_character_class :: Assertion
test_character_class = assertEqual "Could not parse pattern"
  (parseRegex "[ab]")
  (Right
    (Regex [ Quant (CharacterClass [ ClassLiteral 'a', ClassLiteral 'b'])]))

test_negated_character_class :: Assertion
test_negated_character_class = assertEqual "Could not parse pattern"
  (parseRegex "[^a]")
  (Right
    (Regex [ Quant (NegatedCharacterClass [ ClassLiteral 'a' ])]))

test_website_example :: Assertion
test_website_example = assertEqual "Could not parse pattern"
  (parseRegex "[0-9a-f]{32}")
  (Right
    (Regex [ Meta (MinMax (CharacterClass [
      ClassRange (fromJust . orderedRange '0' $ '9'), ClassRange (fromJust . orderedRange 'a' $ 'f')
      ]) (fromJust . positiveOrderedRange 0 $ 32))]))

test_digit :: Assertion
test_digit = assertEqual "Could not parse pattern"
  (parseRegex "\\d")
  (Right (Regex [ Quant (Backslash Digit) ]))

test_transitive_a :: Assertion
test_transitive_a = assertEqual "parsed and rendered version not equal" (Right str) (fmap toText . parseRegex $ str)
  where
    str = "a|a.|([a-b]+)|a{1,3}"

test_escape_metachar :: Assertion
test_escape_metachar = assertEqual "Could not parse pattern"
  (parseRegex "\\*")
  (Right (Regex [ Quant (Character '*') ]))

test_matching_digit :: Assertion
test_matching_digit = do
  s <- aString
  let b = all (\ x -> x `elem` fmap (: "") ("0123456789" :: String)) s
  assertBool "Produced characters that shouldn't match" b
  where
    aString :: IO [String]
    aString = replicateM 10 . generate . matching $ aRegex
    aRegex :: Regex
    aRegex = Regex [ Quant (Backslash Digit) ]

test_matching_produces_valid_matches :: Assertion
test_matching_produces_valid_matches = do
  num <- read . fromMaybe "10" <$> lookupEnv "RM_TEST_VALID_REGEXES"
  regularExpressions <- replicateM num . generate $ arbitrary
  allValid <- mapM validRegex regularExpressions
  assertEqual "All valid" True (and allValid)
  where
    validRegex :: Regex -> IO Bool
    validRegex re = do
      ex <- examples re
      let result = pcreMatch re `all` ex
      -- debugPrint re ex
      if not result then do
        debugPrint re ex
        return result
      else
        return result
    pcreMatch :: Regex -> String -> Bool
    pcreMatch re = (=~ reAsString re)
    examples :: Regex -> IO [String]
    examples = replicateM 5 . generate . matching
    reAsString :: Regex -> String
    reAsString = toS . toText

    debugPrint re ex = do
      setSGR [SetColor Foreground Vivid Red]
      putStr " > "
      setSGR [Reset]
      putStr . reAsString $ re
      setSGR [SetColor Foreground Dull Blue]
      putStr " =~ "
      setSGR [Reset]
      putStrLn $ intercalate "," ex
      putStrLn ""
      hFlush stdout
