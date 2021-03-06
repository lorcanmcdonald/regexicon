module Test.QuickCheck.Regex.PCRE.Types.Quantifiable
where

import Data.Data
import Test.QuickCheck
import Test.QuickCheck.Regex.Exemplify
import Test.QuickCheck.Regex.PCRE.RegexRenderer
import Text.ParserCombinators.Parsec

data Quantifiable

instance Arbitrary Quantifiable
instance Data Quantifiable
instance Eq Quantifiable
instance Exemplify Quantifiable
instance RegexRenderer Quantifiable
instance Show Quantifiable

backslashSequence :: GenParser Char st Quantifiable
