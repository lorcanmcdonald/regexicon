{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.QuickCheck.Regex.PCRE.Types.Ranges
  ( CharacterRange,
    CountRange,
    HasRange (..),
    characterRange,
    countRange,
    inCharacterRange,
    randomCountRange,
  )
where

import Data.Data
import Data.Range
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer

newtype Range' a = Range' (Range a)
  deriving (Show, Eq)

class HasRange r a where
  extractRange :: r -> (a, a)

instance HasRange (Range' a) a where
  extractRange
    ( Range'
        ( SpanRange
            (Bound start Inclusive)
            (Bound end Inclusive)
          )
      ) = (start, end)
  extractRange _ = error "DevError Haven't accounted for all range types"

instance (Typeable a) => Data (Range' a) where
  gunfold _ _ = error "Range' gunfold undefined"
  toConstr _ = error "Range' toConstr undefined"
  dataTypeOf _ = error "Range' dataTypeOf undefined"

instance Exemplify (Range' a) where
  -- examples (CountRange (SpanRange (Bound start Inclusive) (Bound end Inclusive))) =
  examples (Range' _) =
    error "Range' examples undefined"

instance Arbitrary (Range' a) where
  arbitrary = error "Range' arbitrary undefined"

newtype CountRange a = CountRange (Range' a)
  deriving (Data, Show, Eq, Exemplify, Arbitrary)

instance RegexRenderer (CountRange a) where
  render = error "CountRange render undefined"

instance HasRange (CountRange a) a where
  extractRange (CountRange r) = extractRange r

countRange :: Int -> Int -> Maybe (CountRange Int)
countRange start end =
  pure $
    CountRange
      ( Range'
          (SpanRange (Bound start Inclusive) (Bound end Inclusive))
      )

randomCountRange :: CountRange a -> (a, a)
randomCountRange = undefined

newtype CharacterRange = CharacterRange (Range' Char)
  deriving (Data, Show, Eq, Exemplify, Arbitrary)

instance RegexRenderer CharacterRange where
  render = error "CharacterRange render undefined"

characterRange :: Char -> Char-> Maybe CharacterRange
characterRange start end =
  pure $
    CharacterRange
      ( Range'
          (SpanRange (Bound start Inclusive) (Bound end Inclusive))
      )

inCharacterRange :: CharacterRange -> Char -> Bool
inCharacterRange = error "inCharacterRange undefined"

-- module Test.QuickCheck.Regex.PCRE.Types.Ranges
--   ( OrderedRange,
--     PositiveOrderedRange,
--     orderedRange,
--     inRange,
--     constrainedOrderedRange,
--     positiveOrderedRange,
--   )
-- where

-- import Data.Data
-- import Data.Range
-- import Test.QuickCheck
-- import Test.QuickCheck.Regex.Exemplify
-- import Test.QuickCheck.Regex.PCRE.RegexRenderer

-- -- import Test.QuickCheck.Regex.PCRE.RegexRenderer

-- instance Exemplify (OrderedRange a) where
--   examples (MinToInfRange a) = error "no instance Exemplify OrderedRange"
--   examples (ZeroToMaxRange a) = error "no instance Exemplify OrderedRange"
--   examples (BoundedRange a) = error "no instance Exemplify OrderedRange"

-- instance Arbitrary (OrderedRange a) where
--   arbitrary = error "no instance Arbitrary OrderedRange"

-- instance RegexRenderer (OrderedRange a) where
--   render = error "no instance RegexRenderer OrderedRange"

-- orderedRange = undefined

-- data ConstrainedOrderedRange a = ConstrainedOrderedRange a a
--   deriving (Data, Eq, Show)

-- -- instance (RegexRenderer a) => RegexRenderer (PositiveOrderedRange a) where
-- --   render = render' "{" "," "}"

-- -- render' :: (RegexRenderer a) => String -> String -> String -> PositiveOrderedRange a -> String
-- -- render' beg sep end (MinToInfRange m) = beg <> render m <> sep <> end
-- -- render' beg sep end (ZeroToMaxRange m) = beg <> sep <> render m <> end
-- -- render' beg sep end (BoundedRange minValue maxValue) =
-- --   beg <> (render minValue) <> sep <> (render maxValue) <> end

-- data PositiveOrderedRange a = PositiveOrderedRange (OrderedRange a)

-- data OrderedRange a
--   = MinToInfRange (Range a)
--   | ZeroToMaxRange (Range a)
--   | BoundedRange (Range a)
--   deriving (Data, Eq, Show)

-- constrainedOrderedRange :: (Ord a) => a -> a -> Maybe (ConstrainedOrderedRange a)
-- constrainedOrderedRange c d
--   | c <= d = pure $ ConstrainedOrderedRange c d
--   | otherwise = Nothing

-- -- TODO, ordered ranges seem to require a typeclass or typeclasses. They need
-- -- to be able to say that the beginning and ending values are ordered where
-- -- both are given. Then they also need to have an operation `inRange`, which
-- -- can be unconstrained for {a,} etc.
-- --
-- -- These properties are a bit different for what are currently called
-- -- ConstrainedOrderedRange and PositiveOrderedRange (those names are likely
-- -- wrong too, probably should be specialised to handle Character classes and
-- -- quantifiers separately)
-- positiveOrderedRange ::
--   (Ord a, Num a) =>
--   a ->
--   a ->
--   Maybe (PositiveOrderedRange a)
-- positiveOrderedRange c d
--   | 0 <= c && c <= d = pure . PositiveOrderedRange $ BoundedRange (ConstrainedOrderedRange c d)
--   | otherwise = Nothing

-- inRange :: OrderedRange a -> c -> Bool
-- inRange = error "inRange undefined"

-- extractRange :: OrderedRange a -> (a, a)
-- extractRange (MinToInfRange c) = error "messing with infinity"
-- extractRange (ZeroToMaxRange d) = error "messing with Integers"
-- extractRange (BoundedRange c d) = (c, d)

-- extractPositiveRange :: PositiveOrderedRange a -> (a, a)
-- extractPositiveRange (PositiveOrderedRange oRange) = extractRange oRange

-- instance (Arbitrary a, Ord a) => Arbitrary (ConstrainedOrderedRange a) where
--   arbitrary =
--     do
--       a <- arbitrary
--       b <- arbitrary
--       if a <= b
--         then return (ConstrainedOrderedRange a b)
--         else return (ConstrainedOrderedRange b a)
--   shrink _ = []

-- instance (Arbitrary a, Num a, Ord a) => Arbitrary (PositiveOrderedRange a) where
--   arbitrary =
--     do
--       a <- arbitrary
--       b <- arbitrary
--       mRange <-
--         if abs a < abs b
--           then return . positiveOrderedRange (abs a) $ abs b
--           else return . positiveOrderedRange (abs b) $ abs a
--       case mRange of
--         Just r -> return r
--         Nothing ->
--           error "Tried to generate an invalid Arbitrary PositiveOrderedRange a"
--   shrink _ = []
