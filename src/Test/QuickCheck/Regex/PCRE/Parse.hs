{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Parse (parseRegex) where

import Control.Lens.Combinators (_Left, over)
import Data.Default
import Data.Functor (($>))
import Test.QuickCheck.Regex.PCRE.Types
import Text.ParserCombinators.Parsec

parseRegex :: String -> Either String Regex
parseRegex s = do
  re <- over _Left show . parse (regex <* eof) ("input: " <> s) $ s
  resolveBackreferences re re

regex :: GenParser Char st Regex
regex =
  try (StartOfString <$> (string "^" *> rePattern))
    <|> try (EndOfString <$> (rePattern <* string "$"))
    <|> try (StartAndEndOfString <$> (string "^" *> (rePattern <* string "$")))
    <|> try (Regex <$> rePattern)
    <?> "regex"

rePattern :: GenParser Char st Pattern
rePattern =
  try
    ( Alternative
        <$> (many1 regexCharacter <* string "|")
        <*> many1 regexCharacter `sepBy1` string "|"
    )
    <|> try (Alternative <$> many1 regexCharacter <*> pure [])
      <?> "rePattern"

regexCharacter :: GenParser Char st RegexCharacter
regexCharacter =
  try (Quoted <$> (string "\\Q" *> manyTill anyChar (try (string "\\E"))))
    <|> try (Meta <$> metacharacter)
    <|> try (Quant <$> quantifiable)
    <?> "regexCharacter"

character :: GenParser Char st Quantifiable
character =
  Character <$> noneOf "\\^$.[|()?*+{"
    <?> "character"

metacharacter :: GenParser Char st Metacharacter
metacharacter =
  try (ZeroOrMore <$> quantifiable <* string "*")
    <|> try (OneOrMore <$> quantifiable <* string "+")
    <|> try (MinMax <$> quantifiable <*> positiveIntRange)
    <?> "meta character"

constrainedRange ::
  (Ord a, Read a, Default a) =>
  (a -> a -> Maybe b) ->
  GenParser Char st b
constrainedRange constructor =
  try
    ( do
        _ <- string "{"
        a <- many1 digit
        _ <- string "}"
        case constructor (read a) (read a) of
          Just range -> return range
          Nothing -> fail "Could not create ordered range"
    )
    <|> try
      ( do
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
quantifiable =
  try subpattern
    <|> try ambiguousNumberSequence
    <|> try backslashSequence
    <|> try negatedCharacterClass
    <|> try characterClass
    <|> try character
    <|> try (AnyCharacter <$ string ".")
    <?> "Quantifiable"

subpattern :: GenParser Char st Quantifiable
subpattern =
  try
    ( Subpattern
        <$> (string "(" *> rePattern <* string ")")
    )
    <|> try
      ( Subpattern
          <$> (string "()" $> Alternative [] [])
      )
      <?> "Subpattern"

ambiguousNumberSequence :: GenParser Char st Quantifiable
ambiguousNumberSequence =
  try (AmbiguousNumberSequence <$> (string "\\" *> many1 digit))
    <?> "octal or backreference number sequence"

characterClass :: GenParser Char st Quantifiable
characterClass =
  CharacterClass
    <$> (string "[" *> characterClassCharacters)
    <*> (many characterClassCharacters <* string "]")
    <?> "CharacterClass"

negatedCharacterClass :: GenParser Char st Quantifiable
negatedCharacterClass =
  NegatedCharacterClass
    <$> (string "[^" *> characterClassCharacters)
    <*> (many characterClassCharacters <* string "]")
    <?> "NegatedCharacterClass"
