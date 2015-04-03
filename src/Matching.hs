module Matching where
import Control.Exception
import Control.Monad
import Test.QuickCheck.Regex (matching)
import Test.QuickCheck (generate)

matches :: Int -> String -> IO [String]
matches n re = (replicateM n . generate . matching $ re) `catch` failureResult
    where
        failureResult :: SomeException -> IO [String]
        failureResult _ = return []
