{-# LANGUAGE DeriveDataTypeable #-}

module Test.QuickCheck.Regex.PCRE.Types.Backslashes where

import Data.Char
import Data.Data
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify

data BackslashSequence
  = Nonalphanumeric Char
  | BackslashChar
  | Caret
  | Dollar
  | Dot
  | OpenSquareBracket
  | Pipe
  | OpenParens
  | CloseParens
  | QuestionMark
  | Asterisk
  | Plus
  | OpenBrace
  | Hyphen
  | CloseSquareBracket
  | NonprintingAlarm
  | -- | NonprintingCtrlx Char
    NonprintingEscape
  | NonprintingFormFeed
  | NonprintingLineFeed
  | NonprintingCarriageReturn
  | NonprintingTab
  | NonprintingOctalCodeZero Int
  | NonprintingOctalCode Int
  | NonprintingOctalCodeBraces Int
  | NonprintingHexZero
  | NonprintingHexCode Int
  | NonprintingHexCodeBraces Int
  | Digit
  | NonDigit
  | HorizontalWhiteSpace
  | NotHorizontalWhiteSpace
  | WhiteSpace
  | NotWhiteSpace
  | VerticalWhiteSpace
  | NotVerticalWhiteSpace
  | WordCharacter
  | NonWordCharacter
  deriving (Data, Eq, Show)

instance Arbitrary BackslashSequence where
  arbitrary =
    oneof
      [ Nonalphanumeric <$> nonalphanumeric,
        pure BackslashChar,
        pure Caret,
        pure Dollar,
        pure Dot,
        pure OpenSquareBracket,
        pure Pipe,
        pure OpenParens,
        pure CloseParens,
        pure QuestionMark,
        pure Asterisk,
        pure Plus,
        pure OpenBrace,
        pure Hyphen,
        pure CloseSquareBracket,
        pure NonprintingAlarm,
        -- NonprintingCtrlx <$> arbitrary,
        pure NonprintingEscape,
        pure NonprintingFormFeed,
        pure NonprintingLineFeed,
        pure NonprintingCarriageReturn,
        pure NonprintingTab,
        pure NonprintingHexZero,
        octalLiteral,
        octalOrHexLiteral =<< arbitrary,
        pure Digit,
        pure NonDigit,
        -- pure HorizontalWhiteSpace,
        pure NotHorizontalWhiteSpace,
        pure WhiteSpace,
        pure NotWhiteSpace,
        pure VerticalWhiteSpace,
        pure NotVerticalWhiteSpace,
        pure WordCharacter,
        pure NonWordCharacter
      ]
    where
      octalOrHexLiteral :: Positive Int -> Gen BackslashSequence
      octalOrHexLiteral (Positive x) =
        oneof
          [ pure (NonprintingOctalCodeZero x),
            pure (NonprintingOctalCodeBraces x),
            pure (NonprintingHexCode x),
            pure (NonprintingHexCodeBraces x)
          ]
      octalLiteral :: Gen BackslashSequence
      octalLiteral =
        NonprintingHexCodeBraces <$> arbitrary `suchThat` (> 9)

  
  shrink _ = []

nonalphanumeric :: Gen Char
nonalphanumeric =
  arbitraryASCIIChar
    `suchThat` ( \x ->
                   not (isDigit x)
                     && x /= '\NUL'
                     && not (isAsciiUpper x)
                     && not (isAsciiLower x)
               )

instance Exemplify BackslashSequence where
  examples (Nonalphanumeric c) = pure (c : "")
  examples BackslashChar = pure "\\"
  examples Caret = pure "^"
  examples Dollar = pure "$"
  examples Dot = pure "."
  examples OpenSquareBracket = pure "["
  examples Pipe = pure "|"
  examples OpenParens = pure "("
  examples CloseParens = pure ")"
  examples QuestionMark = pure "?"
  examples Asterisk = pure "*"
  examples Plus = pure "+"
  examples OpenBrace = pure "{"
  examples Hyphen = pure "-"
  examples CloseSquareBracket = pure "]"
  examples NonprintingAlarm = pure "\a"
  -- examples (NonprintingCtrlx c) = pure ("\\c" <> [c])
  examples NonprintingEscape = pure "\ESC"
  examples NonprintingFormFeed = pure "\f"
  examples NonprintingLineFeed = pure "\n"
  examples NonprintingCarriageReturn = pure "\r"
  examples NonprintingTab = pure "\t"
  examples (NonprintingOctalCodeZero c) = pure [chr c]
  examples (NonprintingOctalCode c) = pure [chr c]
  examples (NonprintingOctalCodeBraces c) = pure [chr c]
  examples NonprintingHexZero = pure [chr 0]
  examples (NonprintingHexCode c) = pure [chr c]
  examples (NonprintingHexCodeBraces c) = pure [chr c]
  examples Digit =
    oneof
      [ pure "0",
        pure "1",
        pure "2",
        pure "3",
        pure "4",
        pure "5",
        pure "6",
        pure "7",
        pure "8",
        pure "9"
      ]
  examples NonDigit =
    fmap (: "") (arbitraryASCIIChar `suchThat` (not . isDigit))
  examples HorizontalWhiteSpace =
    (: "") <$> oneof (map pure horizontalSpace)
  examples NotHorizontalWhiteSpace =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return horizontalSpace)
  examples VerticalWhiteSpace =
    (: "") <$> oneof (map return verticalSpace)
  examples NotVerticalWhiteSpace =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return verticalSpace)
  examples WhiteSpace =
    (: "") <$> oneof (map return whiteSpace)
  examples NotWhiteSpace =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return whiteSpace)
  examples WordCharacter =
    (: "") <$> oneof (map return wordChar)
  examples NonWordCharacter =
    fmap (: "") arbitraryASCIIChar `suchThat` (`notElem` map return wordChar)

horizontalSpace :: String
horizontalSpace =
  [ '\x0009', -- Horizontal tab (HT)
    '\x0020', -- Space
    '\x00A0', -- Non-break space
    '\x1680', -- Ogham space mark
    '\x180E', -- Mongolian vowel separator
    '\x2000', -- En quad
    '\x2001', -- Em quad
    '\x2002', -- En space
    '\x2003', -- Em space
    '\x2004', -- Three-per-em space
    '\x2005', -- Four-per-em space
    '\x2006', -- Six-per-em space
    '\x2007', -- Figure space
    '\x2008', -- Punctuation space
    '\x2009', -- Thin space
    '\x200A', -- Hair space
    '\x202F', -- Narrow no-break space
    '\x205F', -- Medium mathematical space
    '\x3000' -- Ideographic space
  ]

verticalSpace :: String
verticalSpace =
  [ '\x000A', -- Linefeed (LF)
    '\x000B', -- Vertical tab (VT)
    '\x000C', -- Form feed (FF)
    '\x000D', -- Carriage return (CR)
    '\x0085', -- Next line (NEL)
    '\x2028', -- Line separator
    '\x2029' -- Paragraph separator
  ]

whiteSpace :: String
whiteSpace = horizontalSpace <> verticalSpace

wordChar :: String
wordChar = ['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z']
