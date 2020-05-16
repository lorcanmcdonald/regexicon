{-# LANGUAGE OverloadedStrings #-}
module Matching where
import Control.Exception
import Control.Monad
import Data.Aeson (ToJSON (..), object, (.=))
import Data.String.Conv
import Data.Text (Text)
import Test.QuickCheck (generate)
import Test.QuickCheck.Regex.PCRE (matching, parseRegex)

data RegexResults = RegexTimeout
                  | RegexParseFailure String
                  | RegexResults [Text]

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
matches n reText =
  case parseRegex . toS $ reText of
    Left msg ->
      return . RegexParseFailure $ "Could not parse regular expression: " ++ (toS . show $ msg)
    Right re -> do
      candidates <- (replicateM n . generate . matching $ re) `catch` failureResult
      print . toJSON $ candidates
      return . RegexResults $ fmap toS candidates

  where
    failureResult :: SomeException -> IO [a]
    failureResult _ = return []
