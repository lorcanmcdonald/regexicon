{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.QuickCheck.Regex.PCRE
  ( Regex (..)
  , RegexCharacter (..)
  , MetaCharacter (..)
  , Quantifiable (..)
  , CharacterClassCharacter (..)
  , matching
  , parseRegex
  , toText
  ) where
import Data.Aeson
import Data.Text (Text)
import Test.QuickCheck (Gen)
import Text.ParserCombinators.Parsec

data Regex
  = Regex [RegexCharacter]
  | Alternative Regex Regex
  deriving (Eq, Show)

data RegexCharacter
  = Quant Quantifiable
  | Meta MetaCharacter
  deriving (Eq, Show)

data Quantifiable
  = AnyCharacter
  | Character Char
  | CharacterClass [CharacterClassCharacter]
  | GeneralEscapeCharacter
  | Subpattern Regex
  deriving (Eq, Show)

data MetaCharacter
  = StartOfString
  | EndOfString
  | ZeroOrMore Quantifiable
  | OneOrMore Quantifiable
  deriving (Eq, Show)

data CharacterClassCharacter
  = CharacterClassEscape
  | ClassLiteral Char
  deriving (Eq, Show)

instance ToJSON Regex where
  toJSON = undefined

matching :: Regex -> Gen Regex
matching = error "matching undefined"

toText :: Regex -> Text
toText = error "toText undefined"

parseRegex :: String -> Either ParseError Regex
parseRegex t = parse (regex <* eof) "(unknown)" t

regex :: GenParser Char st Regex
regex = (Regex <$> many1 regexCharacter) `chainl1` (Alternative <$ string "|")

regexCharacter
  = try (Meta <$> metacharacter)
    <|> try (Quant <$> quantifiable)
    <?> "regexCharacter"

character :: GenParser Char st Quantifiable
character = Character <$> noneOf "\\^$.[|()?*+{"
  <?> "character"

metacharacter
  = try (StartOfString <$ string "^")
    <|> try (EndOfString <$ string "$")
    <|> try (ZeroOrMore <$> quantifiable <* string "*")
    <|> try (OneOrMore <$> quantifiable <* string "+")
    <?> "MetaCharacter"

quantifiable
  = try subpattern
    <|> try characterClass
    <|> try (GeneralEscapeCharacter <$ string "\\")
    <|> try character
    <|> try (AnyCharacter <$ string ".")
    <?> "Quantifiable"

subpattern
  = Subpattern <$> (string "(" *> regex <* string ")")

characterClass
  = CharacterClass <$> (string "[" *> many1 characterClassCharacters <* string "]")

characterClassCharacters
  = ClassLiteral <$> noneOf "\\^[]-"
