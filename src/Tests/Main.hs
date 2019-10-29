{-# OPTIONS_GHC -F -pgmF htfpp #-}
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
import Test.Framework
import Test.QuickCheck.Regex.PCRE
import Text.Regex.PCRE hiding (Regex)

main :: IO ()
main = htfMain htf_thisModulesTests

test_empty :: IO ()
test_empty = assertBool . isLeft . parseRegex $ ""

test_single_char :: IO ()
test_single_char =
  assertEqual (Right (Regex [Quant (Character 'a')])) (parseRegex "a")

test_meta_char :: IO ()
test_meta_char =
  assertEqual (Right (Regex [Quant AnyCharacter])) (parseRegex ".")

test_multiple_char :: IO ()
test_multiple_char =
  assertEqual (Right (StartOfString [Quant (Character 'a')])) (parseRegex "^a")

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
  (Right
    (Regex [ Quant (Character 'a'), Quant (Subpattern (Regex [ Quant (Character 'b')]))]))

test_character_class :: IO ()
test_character_class = assertEqual
  (parseRegex "[ab]")
  (Right
    (Regex [ Quant (CharacterClass [ ClassLiteral 'a', ClassLiteral 'b'])]))

test_transitive_a :: IO ()
test_transitive_a = assertEqual (Right str) (fmap toText . parseRegex $ str)
  where
    str = "a|a.|([a-b]+)|a{1,3}"

test_escape_metachar :: IO ()
test_escape_metachar = assertEqual
  (parseRegex "\\*")
  (Right (Regex [ Quant (Character '*') ]))

test_matching_produces_valid_matches :: IO ()
test_matching_produces_valid_matches = do
  num <- read . fromMaybe "10" <$> lookupEnv "RM_TEST_VALID_REGEXES"
  regularExpressions <- replicateM num . generate $ arbitrary
  allValid <- mapM validRegex regularExpressions
  assertEqual True (and allValid)
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
