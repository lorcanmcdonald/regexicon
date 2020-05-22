{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE.Types
  ( CharacterClassCharacter (..)
  , MetaCharacter (..)
  , OrderedRange
  , PositiveOrderedRange
  , Quantifiable (..)
  , Regex (..)
  , RegexCharacter (..)
  , BackslashSequence (..)
  , extractPositiveRange
  , extractRange
  , inCharacterClassCharacter
  , nonalphanumeric
  , orderedRange
  , positiveOrderedRange
  , regexChars
  ) where
import Data.Aeson (ToJSON (..))
import Data.Char
import GHC.Generics
import Test.QuickCheck

data Regex
  = Regex [RegexCharacter]
  | Alternative [RegexCharacter] [RegexCharacter] [[RegexCharacter]]
  | StartOfString [RegexCharacter]
  | EndOfString [RegexCharacter]
  | StartAndEndOfString [RegexCharacter]
  deriving (Eq, Generic, Show)

data RegexCharacter
  = Quant Quantifiable
  | Meta MetaCharacter
  deriving (Eq, Generic, Show)

data Quantifiable
  = AnyCharacter
  | Character Char
  | Backslash BackslashSequence
  | CharacterClass CharacterClassCharacter [CharacterClassCharacter]
  | NegatedCharacterClass CharacterClassCharacter [CharacterClassCharacter]
  | Subpattern [RegexCharacter]
  deriving (Eq, Generic, Show)

data MetaCharacter
  = ZeroOrMore Quantifiable
  | OneOrMore Quantifiable
  | MinMax Quantifiable (PositiveOrderedRange Int)
  deriving (Eq, Generic, Show)

data CharacterClassCharacter
  = ClassLiteral Char
  | ClassRange (OrderedRange Char)
  deriving (Eq, Generic, Show)

data BackslashSequence
  = Nonalphanumeric Char
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
  deriving (Eq, Show)

data OrderedRange a = OrderedRange a a
  deriving (Eq, Show)

newtype PositiveOrderedRange a = PositiveOrderedRange (OrderedRange a)
  deriving (Eq, Generic, Show)

orderedRange :: (Ord a) => a -> a -> Maybe (OrderedRange a)
orderedRange c d
  | c <= d = pure $ OrderedRange c d
  | otherwise = Nothing

positiveOrderedRange
  :: (Ord a, Num a)
  => a -> a -> Maybe (PositiveOrderedRange a)
positiveOrderedRange c d
  | 0 <= c && c <= d = pure $ PositiveOrderedRange (OrderedRange c d)
  | otherwise = Nothing

instance ToJSON Regex where
  toJSON = undefined

instance Arbitrary Regex where
  arbitrary
    = oneof
    [ Regex <$> ((:) <$> arbitrary <*> arbitrary)
    , Alternative <$> arbitrary <*> arbitrary <*> arbitrary
    , StartOfString <$> ((:) <$> arbitrary <*> arbitrary)
    , EndOfString <$> ((:) <$> arbitrary <*> arbitrary)
    , StartAndEndOfString <$> ((:) <$> arbitrary <*> arbitrary)
    ]
  shrink = genericShrink

instance Arbitrary RegexCharacter where
  arbitrary
    = oneof
    [ Quant <$> arbitrary
    , Meta <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary Quantifiable where
  arbitrary = sized quant'
    where
      quant' n | n > 3
        = oneof
        [ pure AnyCharacter
        , Character <$> regexChars
        , Backslash <$> arbitrary
        , CharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary) -- CharacterClass must have at least one element
        , NegatedCharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary) -- NegatedCharacterClass must have at least one element
        ]
      quant' n | n >= 0 && n <= 3 -- Subpattern can cause very deep trees at larger sizes
        = oneof
        [ pure AnyCharacter
        , Character <$> regexChars
        -- , Backslash <$> arbitrary
        , CharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary) -- CharacterClass must have at least one element
        , NegatedCharacterClass <$> arbitrary <*> ((:) <$> arbitrary <*> arbitrary) -- NegatedCharacterClass must have at least one element
        , fmap Subpattern arbitrary
        ]
      quant' _ = pure AnyCharacter
  shrink = genericShrink

instance Arbitrary MetaCharacter where
  arbitrary
    = oneof
    [ ZeroOrMore <$> arbitrary
    , OneOrMore <$> arbitrary
    , MinMax <$> arbitrary <*> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary CharacterClassCharacter where
  arbitrary
    = oneof
    [ ClassLiteral <$> regexChars
    -- , ClassRange <$> arbitrary
    ]
  shrink = genericShrink

instance Arbitrary BackslashSequence where
  arbitrary
    = oneof
    [ Nonalphanumeric <$> nonalphanumeric
    , pure Digit
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
nonalphanumeric = arbitraryASCIIChar `suchThat`
  (\ x
    -> not (isDigit x)
    && x /= '\NUL'
    && not (isAsciiUpper x)
    && not (isAsciiLower x)
  )

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
  shrink = genericShrink

regexChars :: Gen Char
regexChars = oneof [choose ('a', 'z'), choose ('A', 'Z'), choose ('0', '9')] -- TODO Extend to non-metacharacter chars

extractRange :: OrderedRange a -> (a, a)
extractRange (OrderedRange c d) = (c, d)

extractPositiveRange :: PositiveOrderedRange a -> (a, a)
extractPositiveRange (PositiveOrderedRange oRange) = extractRange oRange

inCharacterClassCharacter :: Char -> CharacterClassCharacter -> Bool
inCharacterClassCharacter c (ClassLiteral l) = c == l
inCharacterClassCharacter c (ClassRange r)
  = c >= a && c <= b
  where
    (a, b) = extractRange r

