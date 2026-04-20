module ParseTests where

import Control.Exception (SomeException, catch)
import Control.Monad (replicateM)
import Control.Time (delay)
import Data.Either.Extra (fromRight, isLeft, isRight)
import Data.List (intercalate, isInfixOf)
import Data.List.NonEmpty (fromList, nonEmpty)
import Data.Maybe (fromJust)
import Data.String.Conv (toS)
import Helpers
import System.Console.ANSI
import System.IO (hFlush, stdout)
import Test.QuickCheck.Regex.PCRE
import Test.QuickCheck.Regex.PCRE.Types
import Test.QuickCheck.Regex.PCRE.Types.Types.Ranges
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Text.Regex.PCRE hiding (Regex)

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
        testCase "a?" test_zero_or_one,
        testCase "a{2,}" test_two_or_more,
        testCase
          "[\\d]x"
          ( "[\\d]"
              `shouldBe` Regex
                ( Alternative . fromList $
                    [ RegexCharacterList
                        [ Quant
                            ( CharacterClass . fromJust . nonEmpty $
                                [ fromRight
                                    (error "Left")
                                    (characterClassCharacter "\\d")
                                ]
                            )
                        ]
                    ]
                )
          ),
        testCase
          "[a\\-b]"
          ( "[a\\-b]"
              `shouldBe` Regex
                ( Alternative . fromList $
                    [ RegexCharacterList
                        [ Quant
                            ( CharacterClass . fromJust . nonEmpty $
                                [ fromRight (error "Left a") (characterClassCharacter "a"),
                                  fromRight (error "Left -") (characterClassCharacter "\\-"),
                                  fromRight (error "Left b") (characterClassCharacter "b")
                                ]
                            )
                        ]
                    ]
                )
          ),
        testCase
          "(a|b)"
          ( "(a|b)"
              `shouldBe` Regex
                ( Alternative . fromList $
                    [ RegexCharacterList
                        [ Quant
                            ( Subpattern
                                ( Alternative . fromList $
                                    [ RegexCharacterList [Quant (Character 'a')],
                                      RegexCharacterList [Quant (Character 'b')]
                                    ]
                                )
                            )
                        ]
                    ]
                )
          ),
        testCase
          "[0-9a-f]{32}"
          test_website_example,
        testCase "\\d" test_digit,
        testCase
          "^$"
          ( "^$"
              `shouldBe` StartAndEndOfString (Alternative . fromList $ [RegexCharacterList []])
          ),
        testCase
          "a|b|"
          ( "a|b|"
              `shouldBe` Regex
                ( Alternative . fromList $
                    [ RegexCharacterList
                        [ Quant (Character 'a')
                        ],
                      RegexCharacterList [Quant (Character 'b')],
                      RegexCharacterList []
                    ]
                )
          )
      ],
    testGroup
      "Failure cases"
      [ testCase "empty string" test_empty,
        testCase "*" test_invalid_pattern,
        testCase
          "\\7"
          test_invalid_backref_cannot_be_octal,
        testCase
          "[]"
          test_empty_character_class,
        testCase
          "|a"
          test_empty_first_alternative,
        testCase
          "a||"
          test_double_empty_alternative,
        testCase
          "\\x{1,2}"
          test_hexZero_unquantifiable
      ],
    testGroup
      "Transitive properties"
      [ testProperty "parse with PCRE lib" prop_matching_produces_valid_matches,
        testCase
          "transitive example: a|a. "
          (isTransitive "a|a."),
        testCase
          "transitive example: a|a.|([a-b]+)|a{1,3}\\s\\W\\[\\*\\:\\1\\x0"
          (isTransitive "a|a.|([a-b]+)|a{1,3}\\s\\W\\[\\*\\:\\1\\x0"),
        testCase
          "transitive example: \\x"
          (isTransitive "\\x"),
        testCase
          "transitive example: \\x0"
          (isTransitive "\\x0")
      ]
  ]

isTransitive :: String -> Assertion
isTransitive str =
  assertEqual
    "parsed and rendered version not equal"
    (Right str)
    (fmap toText . parseRegex $ str)

prop_matching_produces_valid_matches :: Regex -> Property
prop_matching_produces_valid_matches regex =
  ioProperty (validRegex regex `catch` handleException regex)
  where
    validRegex :: Regex -> IO Bool
    validRegex re = do
      print re
      ex <- examples re
      let result = filter (not . pcreMatch re) . fmap (<> "\n") $ ex
      debugPrint re ex
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

test_hexZero_unquantifiable :: Assertion
test_hexZero_unquantifiable =
  assertBool "parsed `\\x{1,2}` but it is invalid according to PCRE"
    . isLeft
    . parseRegex
    $ "\\x{1,2}"

test_empty_first_alternative :: Assertion
test_empty_first_alternative =
  assertBool "Parsed empty character class"
    . isLeft
    . parseRegex
    $ "|a"

test_double_empty_alternative :: Assertion
test_double_empty_alternative =
  assertBool "Parsed invalid alternatives"
    . isLeft
    . parseRegex
    $ "a||"

test_empty_character_class :: Assertion
test_empty_character_class =
  assertBool "Parsed empty character class"
    . isLeft
    . parseRegex
    $ "[]"

test_invalid_backref_cannot_be_octal :: Assertion
test_invalid_backref_cannot_be_octal =
  assertBool
    "Referencing a non-existent back ref should fail (\\x where x < 8 should not be interpreted as octal)"
    . isLeft
    . parseRegex
    $ "\\7"

test_invalid_pattern :: Assertion
test_invalid_pattern =
  assertBool "Parsed an invalid pattern"
    . isLeft
    . parseRegex
    $ "*"

test_empty :: Assertion
test_empty = assertBool "Empty pattern doesn't fail" . isRight . parseRegex $ ""

test_digit :: Assertion
test_digit =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "\\d")
    (Right (Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash Digit)]])))

test_website_example :: Assertion
test_website_example =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "[0-9a-f]{32}")
    ( Right
        ( Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Meta
                        ( MinMax
                            ( CharacterClass . fromJust . nonEmpty $
                                [ fromRight (error "Left") (characterClassCharacter "0-9"),
                                  fromRight (error "Left") (characterClassCharacter "a-f")
                                ]
                            )
                            (fromJust . countRange 32 $ 32)
                        )
                    ]
                ]
            )
        )
    )

test_zero_or_more :: Assertion
test_zero_or_more =
  assertEqual
    "Incorrectly parsed pattern"
    ( Right
        (Regex (Alternative . fromList $ [RegexCharacterList [Meta (ZeroOrMore (Character 'a'))]]))
    )
    (parseRegex "a*")

test_zero_or_one :: Assertion
test_zero_or_one =
  assertEqual
    "Incorrectly parsed pattern"
    ( Right
        (Regex (Alternative . fromList $ [RegexCharacterList [Meta (ZeroOrOne (Character 'a'))]]))
    )
    (parseRegex "a?")

test_two_or_more :: Assertion
test_two_or_more =
  assertEqual
    "Incorrectly parsed pattern"
    ( Right
        (Regex (Alternative . fromList $ [RegexCharacterList [Meta (ZeroOrOne (Character 'a'))]]))
    )
    (parseRegex "a{2,}")

test_subpattern :: Assertion
test_subpattern =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "a(b)")
    ( Right
        ( Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Quant (Character 'a'),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList [Quant (Character 'b')]]))
                    ]
                ]
            )
        )
    )

test_single_char :: Assertion
test_single_char =
  assertEqual
    "Incorrectly parsed pattern"
    ( Right
        ( Regex
            ( Alternative . fromList $
                [ RegexCharacterList [Quant (Character 'a')]
                ]
            )
        )
    )
    (parseRegex "a")

test_one_or_more :: Assertion
test_one_or_more =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "a+")
    ( Right
        (Regex (Alternative . fromList $ [RegexCharacterList [Meta (OneOrMore (Character 'a'))]]))
    )

test_match_end :: Assertion
test_match_end =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (EndOfString (Alternative . fromList $ [RegexCharacterList [Quant (Character 'a')]])))
    (parseRegex "a$")

test_multiple_char :: Assertion
test_multiple_char =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (StartOfString (Alternative . fromList $ [RegexCharacterList [Quant (Character 'a')]])))
    (parseRegex "^a")

test_meta_char :: Assertion
test_meta_char =
  assertEqual
    "Incorrectly parsed pattern"
    ( Right
        ( Regex
            ( Alternative
                . fromList
                $ [ RegexCharacterList [Quant AnyCharacter]
                  ]
            )
        )
    )
    (parseRegex ".")

test_escape_normal_character :: Assertion
test_escape_normal_character =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash (Nonalphanumeric '='))]])))
    (parseRegex "\\=")

test_escape_metachar :: Assertion
test_escape_metachar =
  assertEqual
    "Incorrectly parsed pattern"
    (Right (Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash Asterisk)]])))
    (parseRegex "\\*")

test_negated_character_class :: Assertion
test_negated_character_class =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "[^a]")
    ( Right
        ( Regex
            ( Alternative . fromJust . nonEmpty $
                [ RegexCharacterList
                    [ Quant
                        ( NegatedCharacterClass
                            . fromJust
                            . nonEmpty
                            $ [ fromRight
                                  (error "Left ")
                                  (characterClassCharacter "a")
                              ]
                        )
                    ]
                ]
            )
        )
    )

test_character_class :: Assertion
test_character_class =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "[ab]")
    ( Right
        ( Regex
            ( Alternative . fromJust . nonEmpty $
                [ RegexCharacterList
                    [ Quant
                        ( CharacterClass . fromJust . nonEmpty $
                            [ fromRight
                                (error "Left")
                                (characterClassCharacter "a"),
                              fromRight
                                (error "Left")
                                (characterClassCharacter "b")
                            ]
                        )
                    ]
                ]
            )
        )
    )

test_alternatives :: Assertion
test_alternatives =
  assertEqual
    "Incorrectly parsed pattern"
    (parseRegex "a|b")
    ( Right
        ( Regex
            ( Alternative . fromList $
                [ RegexCharacterList [Quant (Character 'a')],
                  RegexCharacterList [Quant (Character 'b')]
                ]
            )
        )
    )
