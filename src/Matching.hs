{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Matching where
import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding)
import GHC.Generics
import Test.QuickCheck (generate)
import Test.QuickCheck.Regex (matching)
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Web.Scotty

newtype RegexResults = RegexResults [String]
  deriving (Parsable, ToJSON)

matches :: Int -> String -> IO RegexResults
matches n re =
  case parseRegex re of
    Left _ -> return $ RegexResults []
    Right _ -> do
      candidates <- (replicateM n . generate . matching $ re) `catch` failureResult
      print . toJSON $ candidates
      return . RegexResults $ candidates

  where
      failureResult :: SomeException -> IO [a]
      failureResult _ = return []
