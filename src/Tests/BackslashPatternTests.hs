module BackslashPatternTests where

import Data.Either.Extra (fromRight)
import Data.List.NonEmpty (fromList, nonEmpty)
import Data.Maybe (fromJust)
import Helpers
import Test.QuickCheck.Regex.PCRE
import Test.QuickCheck.Regex.PCRE.Types
import Test.Tasty
import Test.Tasty.HUnit

bigBackslashPattern :: String
bigBackslashPattern =
  "\\&\\\\\\^\\$\\.\\[\\|\\(\\)\\?\\*\\+\\{\\-\\]\\a\\e\\f\\n\\r\\t\\0\\15\\o{15}\\xAF\\x{AF}\\d\\D\\h\\H\\v\\V\\w\\W"

backslashPatterns :: [TestTree]
backslashPatterns =
  [ testCase
      "\\D"
      ("\\D" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash NonDigit)]])),
    testCase
      "\\h"
      ("\\h" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash HorizontalWhiteSpace)]])),
    testCase
      "\\H"
      ("\\H" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash NotHorizontalWhiteSpace)]])),
    testCase
      "\\s"
      ("\\s" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash WhiteSpace)]])),
    testCase
      "\\S"
      ("\\S" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash NotWhiteSpace)]])),
    testCase
      "\\v"
      ("\\v" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash VerticalWhiteSpace)]])),
    testCase
      "\\V"
      ("\\V" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash NotVerticalWhiteSpace)]])),
    testCase
      "\\w"
      ("\\w" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash WordCharacter)]])),
    testCase
      "\\W"
      ("\\W" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash NonWordCharacter)]])),
    testCase
      "\\Q...*\\E"
      ("\\Q...*\\E" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quoted "...*"]])),
    testCase
      "[\\Q^.\\E]"
      ( "[\\Q^.\\E]"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Quant
                        ( CharacterClass . fromJust
                            . nonEmpty
                            $ [ fromRight
                                  (error "Left")
                                  (characterClassCharacter "\\Q^.\\E")
                              ]
                        )
                    ]
                ]
            )
      ),
    testCase
      "render [\\Q^.\\E]"
      test_render_quotedclassliterals,
    testCase
      "\a"
      ("\a" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Character '\a')]])),
    testCase
      "\\01"
      ("\\01" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash (NonprintingOctalCode 1))]])),
    testCase
      "\\013"
      ("\\013" `shouldBe` Regex (Alternative . fromList $ [RegexCharacterList [Quant (Backslash (NonprintingOctalCode 11))]])),
    testCase
      "\\11"
      ( "\\11"
          `shouldBe` Regex
            (Alternative . fromList $ [RegexCharacterList [Quant (Backslash (NonprintingOctalCode 9))]])
      ),
    testCase
      "()()()()()()()()()()()\\11"
      ( "()()()()()()()()()()()\\11"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (Subpattern (Alternative . fromList $ [RegexCharacterList []])),
                      Quant (BackReference 11 (Alternative . fromList $ [RegexCharacterList []]))
                    ]
                ]
            )
      ),
    testCase
      "\\113"
      ( "\\113"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [Quant (Backslash (NonprintingOctalCode 75))]
                ]
            )
      ),
    testCase
      "\\o{013}"
      ( "\\o{013}"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [Quant (Backslash (NonprintingOctalCodeBraces 11))]
                ]
            )
      ),
    testCase
      "\\xFF"
      ( "\\xFF"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Quant
                        ( Backslash
                            (NonprintingHexCode 255)
                        )
                    ]
                ]
            )
      ),
    testCase
      bigBackslashPattern
      ( bigBackslashPattern
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Quant (Backslash (Nonalphanumeric '&')),
                      Quant (Backslash BackslashChar),
                      Quant (Backslash Caret),
                      Quant (Backslash Dollar),
                      Quant (Backslash Dot),
                      Quant (Backslash OpenSquareBracket),
                      Quant (Backslash Pipe),
                      Quant (Backslash OpenParens),
                      Quant (Backslash CloseParens),
                      Quant (Backslash QuestionMark),
                      Quant (Backslash Asterisk),
                      Quant (Backslash Plus),
                      Quant (Backslash OpenBrace),
                      Quant (Backslash Hyphen),
                      Quant (Backslash CloseSquareBracket),
                      Quant (Backslash NonprintingAlarm),
                      Quant (Backslash NonprintingEscape),
                      Quant (Backslash NonprintingFormFeed),
                      Quant (Backslash NonprintingLineFeed),
                      Quant (Backslash NonprintingCarriageReturn),
                      Quant (Backslash NonprintingTab),
                      Quant (Backslash (NonprintingOctalCode 0)),
                      Quant (Backslash (NonprintingOctalCode 13)),
                      Quant (Backslash (NonprintingOctalCodeBraces 13)),
                      Quant (Backslash (NonprintingHexCode 175)),
                      Quant (Backslash (NonprintingHexCodeBraces 175)),
                      Quant (Backslash Digit),
                      Quant (Backslash NonDigit),
                      Quant (Backslash HorizontalWhiteSpace),
                      Quant (Backslash NotHorizontalWhiteSpace),
                      Quant (Backslash VerticalWhiteSpace),
                      Quant (Backslash NotVerticalWhiteSpace),
                      Quant (Backslash WordCharacter),
                      Quant
                        ( Backslash
                            NonWordCharacter
                        )
                    ]
                ]
            )
      ),
    testCase
      "\\x{FF}"
      ( "\\x{FF}"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [Quant (Backslash (NonprintingHexCodeBraces 255))]
                ]
            )
      ),
    testCase
      "\\0\\x\\015"
      ( "\\0\\x\\015"
          `shouldBe` Regex
            ( Alternative . fromList $
                [ RegexCharacterList
                    [ Quant (Backslash (NonprintingOctalCode 0)),
                      Quant (Backslash NonprintingHexZero),
                      Quant (Backslash (NonprintingOctalCode 13))
                    ]
                ]
            )
      )
  ]

test_render_quotedclassliterals :: Assertion
test_render_quotedclassliterals =
  assertEqual
    "parse and render did not preserve original regex string"
    "[\\Q^.\\E]"
    (toText . fromRight (Regex (Alternative . fromList $ [])) . parseRegex $ "[\\Q^.\\E]")
