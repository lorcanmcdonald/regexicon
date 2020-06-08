{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Types
  ( CharacterClassCharacter (..),
    BackslashSequence (..),
    MetaCharacter (..),
    OrderedRange,
    Pattern (..),
    PositiveOrderedRange,
    Quantifiable (..),
    Regex (..),
    RegexCharacter (..),
    SubpatternContainer (..),
    backslashSequence,
    extractPositiveRange,
    extractRange,
    inCharacterClassCharacter,
    nonalphanumeric,
    orderedRange,
    positiveOrderedRange,
    regexChars,
  )
where

import Control.Lens ((^?), element)
import Control.Lens.Combinators (_Left, over)
import Control.Lens.Plated
import Control.Monad
import Data.Aeson (ToJSON (..))
import Data.Char
import Data.Data
import Data.Data.Lens
import Data.Functor (($>))
import GHC.Generics
import Numeric (readHex, readOct)
import Test.QuickCheck
import Text.ParserCombinators.Parsec
import Text.Read

data Regex
  = Regex Pattern
  | StartOfString Pattern
  | EndOfString Pattern
  | StartAndEndOfString Pattern
  deriving (Data, Eq, Generic, Show)

data Pattern
  = Alternative [RegexCharacter] [[RegexCharacter]]
  deriving (Data, Eq, Show)

data RegexCharacter
  = Quant Quantifiable
  | Meta MetaCharacter
  | Quoted String
  deriving (Data, Eq, Generic, Show)

data Quantifiable
  = AnyCharacter
  | Character Char
  | AmbiguousNumberSequence String
  | Backslash BackslashSequence
  | BackReference Int Pattern
  | CharacterClass CharacterClassCharacter [CharacterClassCharacter]
  | NegatedCharacterClass CharacterClassCharacter [CharacterClassCharacter]
  | Subpattern Pattern
  deriving (Data, Eq, Show)

data MetaCharacter
  = ZeroOrMore Quantifiable
  | OneOrMore Quantifiable
  | MinMax Quantifiable (PositiveOrderedRange Int)
  deriving (Data, Eq, Generic, Show)

data CharacterClassCharacter
  = ClassLiteral Char
  | ClassRange (OrderedRange Char)
  | QuotedClassLiterals String
  deriving (Data, Eq, Generic, Show)

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
  | NonprintingCtrlx Char
  | NonprintingEscape
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

data OrderedRange a = OrderedRange a a
  deriving (Data, Eq, Show)

newtype PositiveOrderedRange a = PositiveOrderedRange (OrderedRange a)
  deriving (Data, Eq, Generic, Show)

class SubpatternContainer a where
  numSubpatterns :: a -> Int
  numSubpatterns = length . subpatterns

  subpatterns :: a -> [Quantifiable]
  resolveBackreferences :: Regex -> a -> Either String a

orderedRange :: (Ord a) => a -> a -> Maybe (OrderedRange a)
orderedRange c d
  | c <= d = pure $ OrderedRange c d
  | otherwise = Nothing

positiveOrderedRange ::
  (Ord a, Num a) =>
  a ->
  a ->
  Maybe (PositiveOrderedRange a)
positiveOrderedRange c d
  | 0 <= c && c <= d = pure $ PositiveOrderedRange (OrderedRange c d)
  | otherwise = Nothing

instance ToJSON Regex where
  toJSON = undefined

instance Arbitrary Regex where
  arbitrary =
    oneof
      [ Regex <$> arbitrary,
        StartOfString <$> arbitrary,
        EndOfString <$> arbitrary,
        StartAndEndOfString <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary Pattern where
  arbitrary =
    oneof
      [Alternative <$> listOf1 arbitrary <*> arbitrary]

instance Plated Regex where
  plate = uniplate

instance Plated Quantifiable where
  plate = uniplate

instance Arbitrary RegexCharacter where
  arbitrary =
    oneof
      [ Quant <$> arbitrary,
        Meta <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary Quantifiable where
  arbitrary = sized quant'
    where
      quant' n
        | n > 3 =
          oneof
            [ pure AnyCharacter,
              Character <$> regexChars,
              Backslash <$> arbitrary,
              CharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary), -- CharacterClass must have at least one element
              NegatedCharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary) -- NegatedCharacterClass must have at least one element
            ]
      quant' n
        | n >= 0 && n <= 3 -- Subpattern can cause very deep trees at larger sizes
          =
          oneof
            [ pure AnyCharacter,
              Character <$> regexChars,
              Backslash <$> arbitrary,
              CharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary), -- CharacterClass must have at least one element
              NegatedCharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary), -- NegatedCharacterClass must have at least one element
              Subpattern <$> arbitrary
            ]
      quant' _ = pure AnyCharacter

-- shrink = genericShrink

instance Arbitrary MetaCharacter where
  arbitrary =
    oneof
      [ ZeroOrMore <$> arbitrary,
        OneOrMore <$> arbitrary,
        MinMax <$> arbitrary <*> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary CharacterClassCharacter where
  arbitrary =
    oneof
      [ ClassLiteral <$> regexChars
      -- , ClassRange <$> arbitrary
      ]
  shrink = genericShrink

instance Arbitrary BackslashSequence where
  arbitrary =
    oneof
      [ Nonalphanumeric <$> nonalphanumeric,
        pure Digit
        -- , pure NonDigit
        -- , pure HorizontalWhiteSpace
        -- , pure NotHorizontalWhiteSpace
        -- , pure WhiteSpace
        -- , pure NotWhiteSpace
        -- , pure VerticalWhiteSpace
        -- , pure NotVerticalWhiteSpace
        -- , pure WordCharacter
        -- , pure NonWordCharacter
      ]

-- shrink = error "nonalphanumeric has invarients which genericShrink does not maintain"

nonalphanumeric :: Gen Char
nonalphanumeric =
  arbitraryASCIIChar
    `suchThat` ( \x ->
                   not (isDigit x)
                     && x /= '\NUL'
                     && not (isAsciiUpper x)
                     && not (isAsciiLower x)
               )

instance (Arbitrary a, Ord a) => Arbitrary (OrderedRange a) where
  arbitrary =
    do
      a <- arbitrary
      b <- arbitrary
      if a <= b
        then return (OrderedRange a b)
        else return (OrderedRange b a)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (PositiveOrderedRange a) where
  arbitrary =
    do
      a <- arbitrary
      b <- arbitrary
      mRange <-
        if abs a < abs b
          then return . positiveOrderedRange (abs a) $ abs b
          else return . positiveOrderedRange (abs b) $ abs a
      case mRange of
        Just r -> return r
        Nothing ->
          error "Tried to generate an invalid Arbitrary PositiveOrderedRange a"
  shrink = genericShrink

regexChars :: Gen Char
regexChars = oneof [choose ('a', 'z'), choose ('A', 'Z'), choose ('0', '9')] -- TODO Extend to non-metacharacter chars

extractRange :: OrderedRange a -> (a, a)
extractRange (OrderedRange c d) = (c, d)

extractPositiveRange :: PositiveOrderedRange a -> (a, a)
extractPositiveRange (PositiveOrderedRange oRange) = extractRange oRange

inCharacterClassCharacter :: Char -> CharacterClassCharacter -> Bool
inCharacterClassCharacter c (ClassLiteral l) = c == l
inCharacterClassCharacter c (ClassRange r) =
  c >= a && c <= b
  where
    (a, b) = extractRange r
inCharacterClassCharacter c (QuotedClassLiterals q) = c `elem` q

instance SubpatternContainer Regex where
  subpatterns (Regex reChars) = subpatterns reChars
  subpatterns (StartOfString reChars) = subpatterns reChars
  subpatterns (EndOfString reChars) = subpatterns reChars
  subpatterns (StartAndEndOfString reChars) = subpatterns reChars

  resolveBackreferences re (Regex reChars) =
    Regex <$> resolveBackreferences re reChars
  resolveBackreferences re (StartOfString reChars) =
    StartOfString <$> resolveBackreferences re reChars
  resolveBackreferences re (EndOfString reChars) =
    EndOfString <$> resolveBackreferences re reChars
  resolveBackreferences re (StartAndEndOfString reChars) =
    StartAndEndOfString <$> resolveBackreferences re reChars

instance SubpatternContainer Pattern where
  subpatterns (Alternative x xs) = concatMap subpatterns (x : xs)
  resolveBackreferences re (Alternative x xs) = do
    x' <- resolveBackreferences re x
    xs' <- mapM (resolveBackreferences re) xs
    return $ Alternative x' xs'

instance SubpatternContainer [RegexCharacter] where
  subpatterns = concatMap subpatterns

  resolveBackreferences re = mapM (resolveBackreferences re)

instance SubpatternContainer RegexCharacter where
  subpatterns (Quant q) = subpatterns q
  subpatterns (Meta m) = subpatterns m
  subpatterns (Quoted _) = []

  resolveBackreferences re (Quant q) = Quant <$> resolveBackreferences re q
  resolveBackreferences re (Meta m) = Meta <$> resolveBackreferences re m
  resolveBackreferences _ self@(Quoted _) = Right self

instance SubpatternContainer MetaCharacter where
  subpatterns (ZeroOrMore q) = subpatterns q
  subpatterns (OneOrMore q) = subpatterns q
  subpatterns (MinMax q _) = subpatterns q

  resolveBackreferences re (ZeroOrMore q) = ZeroOrMore <$> resolveBackreferences re q
  resolveBackreferences re (OneOrMore q) = OneOrMore <$> resolveBackreferences re q
  resolveBackreferences re (MinMax q r) = MinMax <$> resolveBackreferences re q <*> pure r

instance SubpatternContainer Quantifiable where
  subpatterns AnyCharacter = []
  subpatterns (Character _) = []
  subpatterns (AmbiguousNumberSequence _) = []
  subpatterns (Backslash _) = []
  subpatterns (BackReference _ _) = []
  subpatterns (CharacterClass _ _) = []
  subpatterns (NegatedCharacterClass _ _) = []
  subpatterns self@(Subpattern reChars) = self : subpatterns reChars

  resolveBackreferences _ self@AnyCharacter = pure self
  resolveBackreferences _ self@(Character _) = pure self
  resolveBackreferences _ self@(Backslash _) = pure self
  resolveBackreferences _ self@(BackReference _ _) = pure self
  resolveBackreferences _ self@(CharacterClass _ _) = pure self
  resolveBackreferences _ self@(NegatedCharacterClass _ _) = pure self
  resolveBackreferences re (Subpattern reChars) = Subpattern <$> resolveBackreferences re reChars
  resolveBackreferences re (AmbiguousNumberSequence str) =
    case maybeBackref str of
      Nothing -> asBackslash str
      Just a -> Right a
    where
      maybeBackref :: String -> Maybe Quantifiable
      maybeBackref backRefStr = do
        patternIndex <- readMaybe backRefStr
        subpattern <- subpatterns re ^? element (patternIndex - 1)
        case subpattern of
          Subpattern s -> return $ BackReference patternIndex s
          _ -> mzero
      asBackslash :: String -> Either String Quantifiable
      asBackslash backlashStr = over _Left show . parse backslashSequence "" $ ("\\" <> backlashStr)

backslashSequence :: GenParser Char st Quantifiable
backslashSequence =
  Backslash
    <$> ( string "\\"
            *> ( try (string "^" $> Caret)
                   <|> try (string "$" $> Dollar)
                   <|> try (string "." $> Dot)
                   <|> try (string "[" $> OpenSquareBracket)
                   <|> try (string "|" $> Pipe)
                   <|> try (string "(" $> OpenParens)
                   <|> try (string ")" $> CloseParens)
                   <|> try (string "?" $> QuestionMark)
                   <|> try (string "*" $> Asterisk)
                   <|> try (string "+" $> Plus)
                   <|> try (string "{" $> OpenBrace)
                   <|> try (string "-" $> Hyphen)
                   <|> try (string "]" $> CloseSquareBracket)
                   <|> try (string "a" $> NonprintingAlarm)
                   <|> try (NonprintingCtrlx <$> (string "c" *> anyChar))
                   <|> try (string "e" $> NonprintingEscape)
                   <|> try (string "f" $> NonprintingFormFeed)
                   <|> try (string "n" $> NonprintingLineFeed)
                   <|> try (string "r" $> NonprintingCarriageReturn)
                   <|> try (string "t" $> NonprintingTab)
                   <|> try
                     ( do
                         octText <-
                           try (count 3 octDigit)
                             <|> try (count 2 octDigit)
                             <|> try (count 1 octDigit)
                         let (octValue, _) = head . readOct $ octText
                         return . NonprintingOctalCode $ octValue
                     )
                   <|> try
                     ( do
                         _ <- string "o{"
                         octText <- many1 octDigit
                         let (octValue, _) = head . readOct $ octText
                         _ <- string "}"
                         return . NonprintingOctalCodeBraces $ octValue
                     )
                   <|> try
                     ( do
                         _ <- string "x{"
                         hexText <- many1 hexDigit
                         let (hexValue, _) = head . readHex $ hexText
                         _ <- string "}"
                         return . NonprintingHexCodeBraces $ hexValue
                     )
                   <|> try
                     ( do
                         _ <- string "x"
                         hexText <-
                           try (count 2 hexDigit)
                             <|> try (count 1 hexDigit)
                         let hexParse = readHex hexText
                         return . NonprintingHexCode $
                           case hexParse of
                             [] -> 0 :: Int
                             (val, _) : _ -> val
                     )
                   <|> try (string "x" $> NonprintingHexZero)
                   <|> try
                     ( do
                         _ <- string "x{"
                         hexText <-
                           try (count 2 hexDigit)
                             <|> try (count 1 hexDigit)
                         _ <- string "}"
                         let (hexValue, _) = head . readHex $ hexText
                         return . NonprintingHexCodeBraces $ hexValue
                     )
                   <|> try (string "d" $> Digit)
                   <|> try (string "D" $> NonDigit)
                   <|> try (string "h" $> HorizontalWhiteSpace)
                   <|> try (string "H" $> NotHorizontalWhiteSpace)
                   <|> try (string "s" $> WhiteSpace)
                   <|> try (string "S" $> NotWhiteSpace)
                   <|> try (string "v" $> VerticalWhiteSpace)
                   <|> try (string "V" $> NotVerticalWhiteSpace)
                   <|> try (string "w" $> WordCharacter)
                   <|> try (string "W" $> NonWordCharacter)
                   <|> try (string "\\" $> BackslashChar)
                   <|> try (Nonalphanumeric <$> satisfy (not . isAlphaNum))
               )
        )
