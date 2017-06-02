{-# LANGUAGE OverloadedStrings #-}
module Matching where
import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON (..), defaultOptions, genericToEncoding, object, (.=))
import GHC.Generics
import Test.QuickCheck (generate)
import Test.QuickCheck.Regex (matching)
import Text.Regex.TDFA.ReadRegex (parseRegex)
import Web.Scotty
import Data.String.Conv
import Data.Text (Text)

data RegexResults = RegexTimeout
                  | RegexParseFailure String
                  | RegexResults [String]

instance ToJSON RegexResults where
  toJSON RegexTimeout = object
                        [ "message" .= ("Timeout" :: Text)
                        , "type" .= ("RegexTimeout" :: Text)
                        ]
  toJSON (RegexParseFailure message) = object
                                     [ "message" .= message
                                     , "type" .= ("RegexParseFailure" :: Text)
                                     ]
  toJSON (RegexResults candidates) = toJSON candidates

matches :: Int -> String -> IO RegexResults
matches _ "" = return . RegexResults $ []
matches n re =
  case parseRegex re of
    Left msg ->
      return . RegexParseFailure $ "Could not parse regular expression: " ++ (toS . show $ msg)
    Right _ -> do
      candidates <- (replicateM n . generate . matching $ re) `catch` failureResult
      print . toJSON $ candidates
      return . RegexResults $ candidates

  where
    failureResult :: SomeException -> IO [a]
    failureResult _ = return []
