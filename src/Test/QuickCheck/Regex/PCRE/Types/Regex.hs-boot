module Test.QuickCheck.Regex.PCRE.Types.Regex where

import Data.Data
import {-# SOURCE #-} Test.QuickCheck.Regex.SubpatternContainer

data Regex

instance Data Regex
instance Eq Regex
instance Show Regex
instance SubpatternContainer Regex
