{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Test.QuickCheck.Regex.PCRE.Types.Ranges
  ( OrderedRange,
    PositiveOrderedRange,
    extractPositiveRange,
    extractRange,
    orderedRange,
    positiveOrderedRange,
  )
where

import Data.Data
import GHC.Generics
import Test.QuickCheck

data OrderedRange a = OrderedRange a a
  deriving (Data, Eq, Generic, Show)

newtype PositiveOrderedRange a = PositiveOrderedRange (OrderedRange a)
  deriving (Data, Eq, Generic, Show)

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

extractRange :: OrderedRange a -> (a, a)
extractRange (OrderedRange c d) = (c, d)

extractPositiveRange :: PositiveOrderedRange a -> (a, a)
extractPositiveRange (PositiveOrderedRange oRange) = extractRange oRange

instance (Arbitrary a, Ord a) => Arbitrary (OrderedRange a) where
  arbitrary =
    do
      a <- arbitrary
      b <- arbitrary
      if a <= b
        then return (OrderedRange a b)
        else return (OrderedRange b a)
  shrink = genericShrink

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
