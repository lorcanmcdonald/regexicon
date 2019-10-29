{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE.Types
  ( CharacterClassCharacter (..)
  , MetaCharacter (..)
  , OrderedRange
  , PositiveOrderedRange
  , Quantifiable (..)
  , Regex (..)
  , RegexCharacter (..)
  , extractPositiveRange
  , extractRange
  , inCharacterClassCharacter
  , orderedRange
  , positiveOrderedRange
  , regexChars
  ) where
import Control.Monad
import Data.Aeson (ToJSON (..))
import Test.QuickCheck

data Regex
  = Regex [RegexCharacter]
  | Alternative Regex Regex
  | StartOfString [RegexCharacter]
  | EndOfString [RegexCharacter]
  | StartAndEndOfString [RegexCharacter]
  deriving (Eq, Show)

data RegexCharacter
  = Quant Quantifiable
  | Meta MetaCharacter
  deriving (Eq, Show)

data Quantifiable
  = AnyCharacter
  | Character Char
  | CharacterClass [CharacterClassCharacter]
  | NegatedCharacterClass [CharacterClassCharacter]
  | Subpattern Regex
  deriving (Eq, Show)

data MetaCharacter
  = ZeroOrMore Quantifiable
  | OneOrMore Quantifiable
  | MinMax Quantifiable (PositiveOrderedRange Int)
  deriving (Eq, Show)

data CharacterClassCharacter
  = ClassLiteral Char
  | ClassRange (OrderedRange Char)
  deriving (Eq, Show)

data OrderedRange a = OrderedRange a a
  deriving (Eq, Show)

newtype PositiveOrderedRange a = PositiveOrderedRange (OrderedRange a)
  deriving (Eq, Show)

orderedRange :: (Ord a) => a -> a -> Maybe (OrderedRange a)
orderedRange c d
  | c <= d = pure $ OrderedRange c d
  | otherwise = Nothing

positiveOrderedRange :: (Ord a, Num a) => a -> a -> Maybe (PositiveOrderedRange a)
positiveOrderedRange c d
  | 0 <= c && c <= d = pure $ PositiveOrderedRange (OrderedRange c d)
  | otherwise = Nothing

instance ToJSON Regex where
  toJSON = undefined

instance Arbitrary Regex where
  arbitrary
    = oneof
    [ Regex <$> ((:) <$> arbitrary <*> arbitrary)
    , liftM2 Alternative arbitrary arbitrary
    , StartOfString <$> ((:) <$> arbitrary <*> arbitrary)
    , EndOfString <$> ((:) <$> arbitrary <*> arbitrary)
    , StartAndEndOfString <$> ((:) <$> arbitrary <*> arbitrary)
    ]

instance Arbitrary RegexCharacter where
  arbitrary
    = oneof
    [ Quant <$> arbitrary
    , Meta <$> arbitrary
    ]

instance Arbitrary Quantifiable where
  arbitrary = sized quant'
    where
      quant' 0 = Character <$> regexChars
      quant' n | n < 0
        = Character <$> regexChars
      quant' n | n > 0
        = oneof
        [ pure AnyCharacter
        , Character <$> regexChars
        , CharacterClass <$> ((:) <$> arbitrary <*> arbitrary) -- CharacterClass must have at least one element
        , NegatedCharacterClass <$> ((:) <$> arbitrary <*> arbitrary) -- NegatedCharacterClass must have at least one element
        -- , fmap Subpattern arbitrary -- TODO This can trigger infinite loop
        ]

instance Arbitrary MetaCharacter where
  arbitrary
    = oneof
    [ ZeroOrMore <$> arbitrary
    , OneOrMore <$> arbitrary
    , MinMax <$> arbitrary <*> arbitrary
    ]

instance Arbitrary CharacterClassCharacter where
  arbitrary
    = oneof
    [ ClassLiteral <$> regexChars
    -- , ClassRange <$> arbitrary
    ]

instance (Arbitrary a, Ord a) => Arbitrary (OrderedRange a) where
  arbitrary
    = do
    a <- arbitrary
    b <- arbitrary
    if a <= b then
      return (OrderedRange a b)
    else
      return (OrderedRange b a)

instance (Arbitrary a, Num a, Ord a) => Arbitrary (PositiveOrderedRange a) where
  arbitrary
    = do
    a <- arbitrary
    b <- arbitrary
    mRange <- if abs a < abs b
      then return . positiveOrderedRange (abs a) $ abs b
      else return . positiveOrderedRange (abs b) $ abs a
    case mRange of
      Just r -> return r
      Nothing ->
        error "Tried to generate an invalid Arbitrary PositiveOrderedRange a"

regexChars :: Gen Char
regexChars = oneof [choose ('a', 'z'), choose ('A', 'Z'), choose ('0', '9')] -- TODO Extend to non-metacharacter chars

extractRange :: OrderedRange a -> (a, a)
extractRange (OrderedRange c d) = (c, d)

extractPositiveRange :: PositiveOrderedRange a -> (a, a)
extractPositiveRange (PositiveOrderedRange (OrderedRange c d)) = (c, d)

inCharacterClassCharacter :: Char -> CharacterClassCharacter -> Bool
inCharacterClassCharacter c (ClassLiteral l) = c == l
inCharacterClassCharacter c (ClassRange r)
  = c >= a && c <= b
  where
    (a, b) = extractRange r
