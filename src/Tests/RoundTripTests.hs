module RoundTripTests where

import Data.Either.Extra (isRight)
import Test.QuickCheck.Regex.PCRE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | Check that rendering an AST node and parsing it back yields the same AST.
-- Wraps a single Quantifiable in a bare Regex for parsing.
roundTripQuant :: Quantifiable -> Either String Regex
roundTripQuant q = parseRegex (toText (Regex (Alternative [Quant q] [])))

roundTripMeta :: Metacharacter -> Either String Regex
roundTripMeta m = parseRegex (toText (Regex (Alternative [Meta m] [])))

roundTripTests :: [TestTree]
roundTripTests =
  [ testGroup "Implemented (should pass)" implementedTests,
    testGroup "Not yet implemented (should fail)" notImplementedTests
  ]

-- ---------------------------------------------------------------------------
-- Implemented features
-- ---------------------------------------------------------------------------

implementedTests :: [TestTree]
implementedTests =
  [ testProperty "Regex round-trip" prop_regexRoundTrip,
    testProperty "Metacharacter round-trip" prop_metacharacterRoundTrip,
    testProperty "Quantifiable round-trip" prop_quantifiableRoundTrip,
    testGroup
      "ZeroOrOne (?)"
      [ testCase "a?" $ parseRegex "a?" @?= Right (Regex (Alternative [Meta (ZeroOrOne (Character 'a'))] [])),
        testCase "a?? (lazy)" $ assertBool "a?? should parse" (isRight (parseRegex "a??")),
        testCase "(a+)?" $ parseRegex "(a+)?" @?=
          Right (Regex (Alternative [Meta (ZeroOrOne (Subpattern (Alternative [Meta (OneOrMore (Character 'a'))] [])))] []))
      ],
    testGroup
      "Vertical whitespace (\\v / \\V render fix)"
      [ testCase "\\v renders correctly" $
          toText (Regex (Alternative [Quant (Backslash VerticalWhiteSpace)] [])) @?= "\\v",
        testCase "\\V renders correctly" $
          toText (Regex (Alternative [Quant (Backslash NotVerticalWhiteSpace)] [])) @?= "\\V",
        testCase "\\v round-trip" $
          parseRegex "\\v" @?= Right (Regex (Alternative [Quant (Backslash VerticalWhiteSpace)] [])),
        testCase "\\V round-trip" $
          parseRegex "\\V" @?= Right (Regex (Alternative [Quant (Backslash NotVerticalWhiteSpace)] []))
      ]
  ]

prop_regexRoundTrip :: Regex -> Property
prop_regexRoundTrip re =
  parseRegex (toText re) === Right re

prop_metacharacterRoundTrip :: Metacharacter -> Property
prop_metacharacterRoundTrip m =
  let wrapped = Regex (Alternative [Meta m] [])
   in parseRegex (toText wrapped) === Right wrapped

prop_quantifiableRoundTrip :: Quantifiable -> Property
prop_quantifiableRoundTrip q =
  case q of
    AmbiguousNumberSequence _ -> discard -- intermediate parse state, never in final AST
    BackReference _ _ -> discard -- requires subpattern context to round-trip
    _ ->
      let wrapped = Regex (Alternative [Quant q] [])
       in parseRegex (toText wrapped) === Right wrapped

-- ---------------------------------------------------------------------------
-- Not yet implemented — these tests are intentionally RED.
-- Each one documents the desired behaviour for a missing feature.
-- When a feature is implemented, its test should move to implementedTests.
-- ---------------------------------------------------------------------------

notImplementedTests :: [TestTree]
notImplementedTests =
  [ testGroup
      "Anchors"
      [ testCase "\\b word boundary" $ assertBool "\\b should parse" (isRight (parseRegex "\\b")),
        testCase "\\B non-word boundary" $ assertBool "\\B should parse" (isRight (parseRegex "\\B")),
        testCase "\\A start of subject" $ assertBool "\\A should parse" (isRight (parseRegex "\\A")),
        testCase "\\Z end of subject" $ assertBool "\\Z should parse" (isRight (parseRegex "\\Z")),
        testCase "\\z absolute end" $ assertBool "\\z should parse" (isRight (parseRegex "\\z")),
        testCase "\\K reset match start" $ assertBool "\\K should parse" (isRight (parseRegex "\\K"))
      ],
    testGroup
      "Quantifiers"
      [ testCase "{n,} unbounded" $ assertBool "a{2,} should parse" (isRight (parseRegex "a{2,}")),
        testCase "*? lazy zero-or-more" $ assertBool "a*? should parse" (isRight (parseRegex "a*?")),
        testCase "+? lazy one-or-more" $ assertBool "a+? should parse" (isRight (parseRegex "a+?")),
        testCase "{n,m}? lazy min-max" $ assertBool "a{1,3}? should parse" (isRight (parseRegex "a{1,3}?"))
      ],
    testGroup
      "Groups"
      [ testCase "(?:) non-capturing" $ assertBool "(?:a) should parse" (isRight (parseRegex "(?:a)")),
        testCase "(?=) positive lookahead" $ assertBool "(?=a) should parse" (isRight (parseRegex "(?=a)")),
        testCase "(?!) negative lookahead" $ assertBool "(?!a) should parse" (isRight (parseRegex "(?!a)")),
        testCase "(?<=) positive lookbehind" $ assertBool "(?<=a) should parse" (isRight (parseRegex "(?<=a)")),
        testCase "(?<!) negative lookbehind" $ assertBool "(?<!a) should parse" (isRight (parseRegex "(?<!a)"))
      ],
    testGroup
      "Character types"
      [ testCase "\\N not a newline" $ assertBool "\\N should parse" (isRight (parseRegex "\\N")),
        testCase "\\R any newline" $ assertBool "\\R should parse" (isRight (parseRegex "\\R")),
        testCase "\\X grapheme cluster" $ assertBool "\\X should parse" (isRight (parseRegex "\\X"))
      ],
    testGroup
      "Character classes"
      [ testCase "[\\w] word char in class" $ assertBool "[\\w] should parse" (isRight (parseRegex "[\\w]")),
        testCase "[\\s] whitespace in class" $ assertBool "[\\s] should parse" (isRight (parseRegex "[\\s]")),
        testCase "[\\n] newline in class" $ assertBool "[\\n] should parse" (isRight (parseRegex "[\\n]")),
        testCase "[[:alpha:]] POSIX class" $ assertBool "[[:alpha:]] should parse" (isRight (parseRegex "[[:alpha:]]"))
      ]
  ]
