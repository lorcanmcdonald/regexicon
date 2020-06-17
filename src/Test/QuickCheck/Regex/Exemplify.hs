module Test.QuickCheck.Regex.Exemplify where
import Test.QuickCheck
  ( Gen,
  )

class Exemplify a where
  examples :: a -> Gen String
