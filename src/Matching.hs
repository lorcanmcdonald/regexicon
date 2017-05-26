{-# LANGUAGE DeriveGeneric #-}
module Matching where
import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)
import GHC.Generics
import Test.QuickCheck (generate)
import Test.QuickCheck.Regex (matching)

newtype RegexResults = RegexResults [String]
  deriving Generic

instance ToJSON RegexResults where
    toJSON (RegexResults l) = toJSON $ fmap toJSON l

matches :: Int -> String -> IO RegexResults
matches n re = do
  candidates <- (replicateM n . generate . matching $ re) `catch` failureResult
  print . toJSON $ candidates
  return . RegexResults $ candidates

  where
      failureResult :: SomeException -> IO [a]
      failureResult _ = return []
