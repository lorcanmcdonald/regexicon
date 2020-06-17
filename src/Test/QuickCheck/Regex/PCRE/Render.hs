{-# LANGUAGE FlexibleInstances #-}

module Test.QuickCheck.Regex.PCRE.Render where

import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Test.QuickCheck.Regex.PCRE.Types

toText :: Regex -> String
toText = render
