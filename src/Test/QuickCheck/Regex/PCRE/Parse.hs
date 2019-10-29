{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Test.QuickCheck.Regex.PCRE.Parse (parseRegex) where
import Test.QuickCheck.Regex.PCRE.Types
import Text.ParserCombinators.Parsec

parseRegex :: String -> Either ParseError Regex
parseRegex = parse (regex <* eof) "(unknown)"

regex :: GenParser Char st Regex
regex = try (StartOfString <$> (string "^" *> many1 regexCharacter))
    -- <|> try ((EndOfString <$> regex) <* string "$")
    <|> try (StartAndEndOfString <$> (string "^" *> (many regexCharacter <* string "$")))
    <|> ((Regex <$> many1 regexCharacter) `chainl1` (Alternative <$ string "|"))

regexCharacter :: GenParser Char st RegexCharacter
regexCharacter
  = try (Meta <$> metacharacter)
    <|> try (Quant <$> quantifiable)
    <?> "regexCharacter"

character :: GenParser Char st Quantifiable
character = Character <$> noneOf "\\^$.[|()?*+{"
  <?> "character"

escapedCharacter :: GenParser Char st Quantifiable
escapedCharacter = Character <$> (string "\\" *> oneOf "\\^$.[|()?*+{")
  <?> "escapedCharacter"


metacharacter :: GenParser Char st MetaCharacter
metacharacter
  = try (ZeroOrMore <$> quantifiable <* string "*")
    <|> try (OneOrMore <$> quantifiable <* string "+")
    <|> try (MinMax <$> quantifiable <*> positiveIntRange)
    <?> "MetaCharacter"

positiveIntRange :: GenParser Char st (PositiveOrderedRange Int)
positiveIntRange = try (do
  _ <- string "{"
  a <- many1 digit
  _ <- string ","
  b <- many1 digit
  _ <- string "}"
  case positiveOrderedRange (read a) (read b) of
    Just range -> return range
    Nothing -> fail "Could not create ordered range"
  )

intRange :: GenParser Char st (OrderedRange Int)
intRange = try (do
  _ <- string "{"
  a <- many1 digit
  _ <- string ","
  b <- many1 digit
  _ <- string "}"
  case orderedRange (read a) (read b) of
    Just range -> return range
    Nothing -> fail "Could not create ordered range"
  )

quantifiable :: GenParser Char st Quantifiable
quantifiable
  = try subpattern
    <|> try characterClass
    <|> try character
    <|> try escapedCharacter
    <|> try (AnyCharacter <$ string ".")
    <?> "Quantifiable"

subpattern :: GenParser Char st Quantifiable
subpattern
  = Subpattern <$> (string "(" *> regex <* string ")")
    <?> "Subpattern"

characterClass :: GenParser Char st Quantifiable
characterClass
  = CharacterClass <$> (string "[" *> many1 characterClassCharacters <* string "]")
    <?> "CharacterClass"

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
