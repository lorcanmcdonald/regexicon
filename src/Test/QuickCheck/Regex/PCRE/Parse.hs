{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Parse (parseRegex, rePattern) where

import Control.Lens.Combinators (_Left, over)
import Control.Monad (mzero)
import Data.Default
import Data.List.NonEmpty
import Test.QuickCheck.Regex.PCRE.Types
import Test.QuickCheck.Regex.PCRE.Types.Ranges
import Text.ParserCombinators.Parsec

parseRegex :: String -> Either String Regex
parseRegex s = do
  re <- over _Left show . parse (regex <* eof) ("input: " <> s) $ s
  let patterns = subpatterns re
  patterns' <- subpatterns <$> resolveBackreferences patterns re
  resolveBackreferences patterns' re

regex :: GenParser Char st Regex
regex =
  try (StartAndEndOfString <$> (string "^" *> (rePattern <* string "$")))
    <|> try (EndOfString <$> (rePattern <* string "$"))
    <|> try (StartOfString <$> (string "^" *> rePattern))
    <|> try (Regex <$> rePattern)
    <?> "regex"

rePattern :: GenParser Char st Pattern
rePattern =
  try
    ( do
        chars <- many1 regexCharacter `endBy` string "|"
        case nonEmpty $ RegexCharacterList <$> chars of
          Just cs -> return $ Alternative (cs <> fromList [RegexCharacterList []]) -- Where an "|" ends a regex we need to add an extra [], so that we don't lose that information
          Nothing -> mzero
    )
    <|> try
      ( do
          chars <- many1 regexCharacter `sepBy` string "|"
          case nonEmpty $ RegexCharacterList <$> chars of
            Just cs -> return $ Alternative cs
            Nothing -> mzero
      )
    <|> try
      ( do
          chars <- many regexCharacter
          return . Alternative . fromList $ [RegexCharacterList chars] -- We're passing a list with one element here, so fromList is safe
      )
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
    <|> try (ZeroOrOne <$> quantifiable <* string "?")
    <|> try (OneOrMore <$> quantifiable <* string "+")
    <|> try (MinMax <$> quantifiable <*> positiveIntRange)
    <?> "meta character"

-- minRange :: GenParser Char st Int
-- minRange =
--   try
--     ( do
--         _ <- string "{"
--         a <- many1 digit
--         _ <- string ",}"
--         case (read a :: Maybe Int) of
--           Nothing -> fail "Could not parse range"
--           Just x -> return $ Min x
--     )

-- maxRange :: GenParser Char st Int
-- maxRange = do
--   _ <- string "{,"
--   a <- many1 digit
--   _ <- string "}"
--   case (readMay a :: Maybe Int) of
--     Nothing -> fail "Could not parse range"
--     Just x -> Max <$> x

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

positiveIntRange :: GenParser Char st (CountRange Int)
positiveIntRange = constrainedRange countRange

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
      ( do
          _ <- string "()"
          pure $
            Subpattern (Alternative . fromList $ [RegexCharacterList []])
      )
      <?> "Subpattern"

ambiguousNumberSequence :: GenParser Char st Quantifiable
ambiguousNumberSequence =
  try (AmbiguousNumberSequence <$> (string "\\" *> many1 digit))
    <?> "octal or backreference number sequence"

characterClass :: GenParser Char st Quantifiable
characterClass =
  ( do
      _ <- string "["
      chars <- many1 characterClassCharacters
      _ <- string "]"
      case nonEmpty chars of
        Just cs -> return $ CharacterClass cs
        Nothing -> mzero
  )
    <?> "CharacterClass"

negatedCharacterClass :: GenParser Char st Quantifiable
negatedCharacterClass =
  ( do
      _ <- string "[^"
      chars <- many1 characterClassCharacters
      _ <- string "]"
      case nonEmpty chars of
        Just cs -> return $ NegatedCharacterClass cs
        Nothing -> mzero
  )
    <?> "NegatedCharacterClass"
