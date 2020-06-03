{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (SomeException, catch)
import Control.Monad
import Control.Time
import Data.Either.Extra (fromRight, isLeft)
import Data.List (intercalate, isInfixOf)
import Data.Maybe
import Data.String.Conv
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Test.QuickCheck.Regex.PCRE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Regex.PCRE hiding (Regex)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    ""
    [ testGroup "Parse" parseTests,
      testGroup
        "Matching"
        [ testCase "\\d" test_matching_digit
        ]
    ]

parseTests :: [TestTree]
parseTests =
  [ testGroup
      "Simple patterns"
      [ testCase "a|b" test_alternatives,
        testCase "[ab]" test_character_class,
        testCase "[^a]" test_negated_character_class,
        testCase "\\*" test_escape_metachar,
        testCase "\\=" test_escape_normal_character,
        testCase "." test_meta_char,
        testCase "^a" test_multiple_char,
        testCase "a$" test_match_end,
        testCase "a+" test_one_or_more,
        testCase "a" test_single_char,
        testCase "a(b)" test_subpattern,
        testCase "a*" test_zero_or_more,
        testCase "[0-9a-f]{32}" test_website_example,
        testCase "\\d" test_digit
      ],
    testGroup
      "Backslash patterns"
      [ testCase
          "\\D"
          ("\\D" `shouldBe` Regex [Quant (Backslash NonDigit)]),
        testCase
          "\\h"
          ("\\h" `shouldBe` Regex [Quant (Backslash HorizontalWhiteSpace)]),
        testCase
          "\\H"
          ("\\H" `shouldBe` Regex [Quant (Backslash NotHorizontalWhiteSpace)]),
        testCase
          "\\s"
          ("\\s" `shouldBe` Regex [Quant (Backslash WhiteSpace)]),
        testCase
          "\\S"
          ("\\S" `shouldBe` Regex [Quant (Backslash NotWhiteSpace)]),
        testCase
          "\\v"
          ("\\v" `shouldBe` Regex [Quant (Backslash VerticalWhiteSpace)]),
        testCase
          "\\V"
          ("\\V" `shouldBe` Regex [Quant (Backslash NotVerticalWhiteSpace)]),
        testCase
          "\\w"
          ("\\w" `shouldBe` Regex [Quant (Backslash WordCharacter)]),
        testCase
          "\\W"
          ("\\W" `shouldBe` Regex [Quant (Backslash NonWordCharacter)]),
        testCase
          "\\Q...*\\E"
          ("\\Q...*\\E" `shouldBe` Regex [Quoted "...*"]),
        testCase
          "[\\Q^.\\E]"
          ("[\\Q^.\\E]" `shouldBe` Regex [Quant (CharacterClass (QuotedClassLiterals ['^', '.']) [])]),
        testCase
          "render [\\Q^.\\E]"
          test_render_quotedclassliterals,
        testCase
          "\a"
          ("\a" `shouldBe` Regex [Quant (Character '\a')]),
        testCase
          "\\01"
          ("\\01" `shouldBe` Regex [Quant (Backslash (NonprintingOctalCode 1))]),
        testCase
          "\\013"
          ("\\013" `shouldBe` Regex [Quant (Backslash (NonprintingOctalCode 11))]),
        testCase
          "\\113"
          ("\\113" `shouldBe` Regex [Quant (Backslash (NonprintingOctalCode 75))]),
        testCase
          "\\o{013}"
          ("\\o{013}" `shouldBe` Regex [Quant (Backslash (NonprintingOctalCodeBraces 11))]),
        testCase
          "\\xFF"
          ("\\xFF" `shouldBe` Regex [Quant (Backslash (NonprintingHexCode 255))]),
        testCase
          "\\x{FF}"
          ("\\x{FF}" `shouldBe` Regex [Quant (Backslash (NonprintingHexCodeBraces 255))]),
        testCase
          "\\0\\x\\015"
          ( "\\0\\x\\015"
              `shouldBe` Regex
                [ Quant (Backslash (NonprintingOctalCode 0)),
                  Quant (Backslash NonprintingHexZero),
                  Quant (Backslash (NonprintingOctalCode 13))
                ]
          )
      ],
    testGroup
      "Failure cases"
      [ testCase "empty string" test_empty,
        testCase "*" test_invalid_pattern,
        testCase "[]" test_empty_character_class
      ],
    testGroup
      "Transitive properties"
      [ testProperty "parse with PCRE lib" prop_matching_produces_valid_matches,
        testCase
          "transitive example: a|a. "
          (isTransitive "a|a."),
        testCase
          "transitive example: a|a.|([a-b]+)|a{1,3}\\s\\W\\[\\*\\:"
          (isTransitive "a|a.|([a-b]+)|a{1,3}\\s\\W\\[\\*\\:"),
        testCase
          "transitive example: \\x"
          (isTransitive "\\x"),
        testCase
          "transitive example: \\x0"
          (isTransitive "\\x0")
      ]
  ]

test_empty :: Assertion
test_empty = assertBool "Empty pattern doesn't fail" . isLeft . parseRegex $ ""

test_single_char :: Assertion
test_single_char =
  assertEqual "Incorrectly parsed pattern" (Right (Regex [Quant (Character 'a')])) (parseRegex "a")

test_meta_char :: Assertion
test_meta_char =
  assertEqual "Incorrectly parsed pattern" (Right (Regex [Quant AnyCharacter])) (parseRegex ".")

test_multiple_char :: Assertion
test_multiple_char =
  assertEqual "Incorrectly parsed pattern" (Right (StartOfString [Quant (Character 'a')])) (parseRegex "^a")

test_match_end :: Assertion
test_match_end =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (EndOfString [Quant (Character 'a')]))
    (parseRegex "a$")

test_invalid_pattern :: Assertion
test_invalid_pattern =
  assertBool "Parsed an invalid pattern"
    . isLeft
    . parseRegex
    $ "*"

test_empty_character_class :: Assertion
test_empty_character_class =
  assertBool "Parsed empty character class"
    . isLeft
    . parseRegex
    $ "[]"

test_alternatives :: Assertion
test_alternatives =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "a|b")
    ( Right
        ( Alternative
            [Quant (Character 'a')]
            [Quant (Character 'b')]
            []
        )
    )

test_zero_or_more :: Assertion
test_zero_or_more =
  assertEqual
    "Incorrectly parsed pattern"
    ( Right
        (Regex [Meta (ZeroOrMore (Character 'a'))])
    )
    (parseRegex "a*")

test_one_or_more :: Assertion
test_one_or_more =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "a+")
    ( Right
        (Regex [Meta (OneOrMore (Character 'a'))])
    )

test_subpattern :: Assertion
test_subpattern =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "a(b)")
    ( Right
        (Regex [Quant (Character 'a'), Quant (Subpattern [Quant (Character 'b')])])
    )

test_character_class :: Assertion
test_character_class =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "[ab]")
    ( Right
        (Regex [Quant (CharacterClass (ClassLiteral 'a') [ClassLiteral 'b'])])
    )

test_negated_character_class :: Assertion
test_negated_character_class =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "[^a]")
    ( Right
        (Regex [Quant (NegatedCharacterClass (ClassLiteral 'a') [])])
    )

test_website_example :: Assertion
test_website_example =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "[0-9a-f]{32}")
    ( Right
        ( Regex
            [ Meta
                ( MinMax
                    ( CharacterClass
                        (ClassRange (fromJust . orderedRange '0' $ '9'))
                        [ ClassRange (fromJust . orderedRange 'a' $ 'f')
                        ]
                    )
                    (fromJust . positiveOrderedRange 0 $ 32)
                )
            ]
        )
    )

test_digit :: Assertion
test_digit =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "\\d")
    (Right (Regex [Quant (Backslash Digit)]))

shouldBe :: String -> Regex -> Assertion
shouldBe s regex =
  assertEqual
    "Incorrectly parsed pattern"
    (Right regex)
    (parseRegex s)

isTransitive :: String -> Assertion
isTransitive str =
  assertEqual
    "parsed and rendered version not equal"
    (Right str)
    (fmap toText . parseRegex $ str)

test_escape_metachar :: Assertion
test_escape_metachar =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (Regex [Quant (Backslash Asterisk)]))
    (parseRegex "\\*")

test_escape_normal_character :: Assertion
test_escape_normal_character =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (Regex [Quant (Backslash (Nonalphanumeric '='))]))
    (parseRegex "\\=")

test_matching_digit :: Assertion
test_matching_digit = do
  s <- aString
  let b = all (\x -> x `elem` fmap (: "") ("0123456789" :: String)) s
  assertBool "Produced characters that shouldn't match" b
  where
    aString :: IO [String]
    aString = replicateM 10 . generate . matching $ aRegex
    aRegex :: Regex
    aRegex = Regex [Quant (Backslash Digit)]

test_render_quotedclassliterals :: Assertion
test_render_quotedclassliterals =
  assertEqual
    "parse and render did not preserve original regex string"
    "[\\Q^.\\E]"
    (toText . fromRight (Regex []) . parseRegex $ "[\\Q^.\\E]")

prop_matching_produces_valid_matches :: Regex -> Property
prop_matching_produces_valid_matches regex =
  ioProperty (validRegex regex `catch` handleException regex)
  where
    validRegex :: Regex -> IO Bool
    validRegex re = do
      ex <- examples re
      let result = filter (not . pcreMatch re) . fmap (<> "\n") $ ex
      -- debugPrint re ex
      if not (null result)
        then do
          debugPrint re result
          return False
        else return True
    pcreMatch :: Regex -> String -> Bool
    pcreMatch re = (=~ reAsString re)
    examples :: Regex -> IO [String]
    examples = replicateM 5 . generate . matching
    reAsString :: Regex -> String
    reAsString = toS . toText
    handleException :: Regex -> SomeException -> IO Bool
    handleException regexEx x = do
      let str = show x
      if "ReturnCode (-8)" `isInfixOf` str
        || "regular expression is too large" `isInfixOf` str
        then -- The PCRE return code PCRE_ERROR_MATCHLIMIT maps to -8. This
        -- indicates that the regex passed is taking too long to execute. We're
        -- going to ignore that as a concern as the web interface already has
        -- a guard to ensure that we don't spend too long responding to a
        -- request
        --
        -- Similarly if Quickcheck produces a very large regular expression,
        -- we'll just ignore the fact that the PCRE library wouldn't handle it
          return True
        else do
          putStrLn . toS . toText $ regexEx
          print x
          return False
    debugPrint re ex = do
      delay (1 :: Integer)
      setSGR [SetColor Foreground Vivid Red]
      putStr " > "
      setSGR [Reset]
      putStr . reAsString $ re
      setSGR [SetColor Foreground Dull Blue]
      putStr " =~ "
      setSGR [Reset]
      setSGR [SetColor Background Dull Blue]
      putStrLn $ intercalate "\n • " ex
      setSGR [Reset]
      putStrLn ""
      hFlush stdout
