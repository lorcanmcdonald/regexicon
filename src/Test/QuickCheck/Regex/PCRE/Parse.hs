{-# LANGUAGE OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE.Parse (parseRegex) where
import Data.Char
import Data.Default
import Test.QuickCheck.Regex.PCRE.Types
import Text.ParserCombinators.Parsec
import Data.Functor (($>))

parseRegex :: String -> Either ParseError Regex
parseRegex = parse (regex <* eof) "(unknown)"

regex :: GenParser Char st Regex
regex = try (StartOfString <$> (string "^" *> many1 regexCharacter))
    <|> try (EndOfString <$> (many1 regexCharacter <* string "$"))
    <|> try (StartAndEndOfString <$> (string "^" *> (many regexCharacter <* string "$")))
    <|> try (Alternative
        <$> (many1 regexCharacter <* string "|")
        <*> (many1 regexCharacter <* string "|")
        <*> (many1 regexCharacter `sepBy` string "|"))
    <|> try (Alternative
        <$> (many1 regexCharacter <* string "|")
        <*> many1 regexCharacter
        <*> return [])
    <|> try (Regex <$> many1 regexCharacter)
    <?> "regex"

regexCharacter :: GenParser Char st RegexCharacter
regexCharacter
  = try (Meta <$> metacharacter)
    <|> try (Quant <$> quantifiable)
    <?> "regexCharacter"

character :: GenParser Char st Quantifiable
character = Character <$> noneOf "\\^$.[|()?*+{"
  <?> "character"

escapedCharacter :: GenParser Char st Quantifiable
escapedCharacter = Character <$> (string "\\" *> satisfy (not . isAlphaNum))
  <?> "escapedCharacter"

metacharacter :: GenParser Char st MetaCharacter
metacharacter
  = try (ZeroOrMore <$> quantifiable <* string "*")
    <|> try (OneOrMore <$> quantifiable <* string "+")
    <|> try (MinMax <$> quantifiable <*> positiveIntRange)
    <?> "MetaCharacter"

constrainedRange
  :: (Ord a, Read a, Default a)
  => (a -> a -> Maybe b) -> GenParser Char st b
constrainedRange constructor
  = try (do
    _ <- string "{"
    a <- many1 digit
    _ <- string "}"
    case constructor def (read a) of
      Just range -> return range
      Nothing -> fail "Could not create ordered range"
  )
  <|> try (do
    _ <- string "{"
    a <- many1 digit
    _ <- string ","
    b <- many1 digit
    _ <- string "}"
    case constructor (read a) (read b) of
      Just range -> return range
      Nothing -> fail "Could not create ordered range"
  )

positiveIntRange :: GenParser Char st (PositiveOrderedRange Int)
positiveIntRange = constrainedRange positiveOrderedRange

quantifiable :: GenParser Char st Quantifiable
quantifiable
  = try subpattern
    <|> try backslashSequence
    <|> try negatedCharacterClass
    <|> try characterClass
    <|> try escapedCharacter
    <|> try character
    <|> try (AnyCharacter <$ string ".")
    <?> "Quantifiable"

subpattern :: GenParser Char st Quantifiable
subpattern
  = Subpattern <$> (string "(" *> many1 regexCharacter <* string ")")
    <?> "Subpattern"

backslashSequence :: GenParser Char st Quantifiable
backslashSequence
  = Backslash <$> (string "\\d" $> Digit)

characterClass :: GenParser Char st Quantifiable
characterClass
  = CharacterClass
    <$> (string "[" *> characterClassCharacters)
    <*> (many characterClassCharacters <* string "]")
    <?> "CharacterClass"

negatedCharacterClass :: GenParser Char st Quantifiable
negatedCharacterClass
  = NegatedCharacterClass
    <$> (string "[^" *> characterClassCharacters)
    <*> (many characterClassCharacters <* string "]")
    <?> "NegatedCharacterClass"

characterClassCharacters :: GenParser Char st CharacterClassCharacter
characterClassCharacters
  = try (do
    a <- validChars
    _ <- string "-"
    b <- validChars
    case orderedRange a b of
      Just range -> return $ ClassRange range
      Nothing -> fail "Range out of order in charcter class"
  )
  <|> try (ClassLiteral <$> validChars)
  <?> "CharacterClassCharacter"
  where
    validChars = noneOf "\\^[]-"
