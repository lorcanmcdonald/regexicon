module Test.QuickCheck.Regex.PCRE.Types.Pattern  where

import Data.Data
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer

data Pattern

instance Arbitrary Pattern
instance Data Pattern
instance Eq Pattern
instance Exemplify Pattern
instance RegexRenderer Pattern
instance Show Pattern
