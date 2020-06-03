{-# LANGUAGE OverloadedStrings #-}

module Test.QuickCheck.Regex.PCRE.Parse (parseRegex) where

import Data.Char
import Data.Default
import Data.Functor (($>))
import Numeric (readHex, readOct)
import Test.QuickCheck.Regex.PCRE.Types
import Text.ParserCombinators.Parsec

parseRegex :: String -> Either ParseError Regex
parseRegex = parse (regex <* eof) "(unknown)"

regex :: GenParser Char st Regex
regex =
  try (StartOfString <$> (string "^" *> many1 regexCharacter))
    <|> try (EndOfString <$> (many1 regexCharacter <* string "$"))
    <|> try (StartAndEndOfString <$> (string "^" *> (many regexCharacter <* string "$")))
    <|> try
      ( Alternative
          <$> (many1 regexCharacter <* string "|")
          <*> (many1 regexCharacter <* string "|")
          <*> (many1 regexCharacter `sepBy` string "|")
      )
    <|> try
      ( Alternative
          <$> (many1 regexCharacter <* string "|")
          <*> many1 regexCharacter
          <*> return []
      )
    <|> try (Regex <$> many1 regexCharacter)
    <?> "regex"

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

metacharacter :: GenParser Char st MetaCharacter
metacharacter =
  try (ZeroOrMore <$> quantifiable <* string "*")
    <|> try (OneOrMore <$> quantifiable <* string "+")
    <|> try (MinMax <$> quantifiable <*> positiveIntRange)
    <?> "MetaCharacter"

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
        case constructor def (read a) of
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
    <|> try backslashSequence
    <|> try negatedCharacterClass
    <|> try characterClass
    <|> try character
    <|> try (AnyCharacter <$ string ".")
    <?> "Quantifiable"

subpattern :: GenParser Char st Quantifiable
subpattern =
  Subpattern <$> (string "(" *> many1 regexCharacter <* string ")")
    <?> "Subpattern"

backslashSequence :: GenParser Char st Quantifiable
backslashSequence =
  Backslash
    <$> ( string "\\"
            *> ( try (string "^" $> Caret)
                   <|> try (string "$" $> Dollar)
                   <|> try (string "." $> Dot)
                   <|> try (string "[" $> OpenSquareBracket)
                   <|> try (string "|" $> Pipe)
                   <|> try (string "(" $> OpenParens)
                   <|> try (string ")" $> CloseParens)
                   <|> try (string "?" $> QuestionMark)
                   <|> try (string "*" $> Asterisk)
                   <|> try (string "+" $> Plus)
                   <|> try (string "{" $> OpenBrace)
                   <|> try (string "-" $> Hyphen)
                   <|> try (string "]" $> CloseSquareBracket)
                   <|> try (string "a" $> NonprintingAlarm)
                   <|> try (NonprintingCtrlx <$> (string "c" *> anyChar))
                   <|> try (string "e" $> NonprintingEscape)
                   <|> try (string "f" $> NonprintingFormFeed)
                   <|> try (string "n" $> NonprintingLineFeed)
                   <|> try (string "r" $> NonprintingCarriageReturn)
                   <|> try (string "t" $> NonprintingTab)
                   <|> try
                     ( do
                         octText <-
                           try (count 3 octDigit)
                             <|> try (count 2 octDigit)
                             <|> try (count 1 octDigit)
                         let (octValue, _) = head . readOct $ octText
                         return . NonprintingOctalCode $ octValue
                     )
                   <|> try
                     ( do
                         _ <- string "o{"
                         octText <- many1 octDigit
                         let (octValue, _) = head . readOct $ octText
                         _ <- string "}"
                         return . NonprintingOctalCodeBraces $ octValue
                     )
                   <|> try
                     ( do
                         _ <- string "x{"
                         hexText <- many1 hexDigit
                         let (hexValue, _) = head . readHex $ hexText
                         _ <- string "}"
                         return . NonprintingHexCodeBraces $ hexValue
                     )
                   <|> try
                     ( do
                         _ <- string "x"
                         hexText <-
                           try (count 2 hexDigit)
                             <|> try (count 1 hexDigit)
                         let hexParse = readHex hexText
                         return . NonprintingHexCode $
                           case hexParse of
                             [] -> 0 :: Int
                             (val, _) : _ -> val
                     )
                   <|> try (string "x" $> NonprintingHexZero)
                   <|> try
                     ( do
                         _ <- string "x{"
                         hexText <-
                           try (count 2 hexDigit)
                             <|> try (count 1 hexDigit)
                         _ <- string "}"
                         let (hexValue, _) = head . readHex $ hexText
                         return . NonprintingHexCodeBraces $ hexValue
                     )
                   <|> try (string "d" $> Digit)
                   <|> try (string "D" $> NonDigit)
                   <|> try (string "h" $> HorizontalWhiteSpace)
                   <|> try (string "H" $> NotHorizontalWhiteSpace)
                   <|> try (string "s" $> WhiteSpace)
                   <|> try (string "S" $> NotWhiteSpace)
                   <|> try (string "v" $> VerticalWhiteSpace)
                   <|> try (string "V" $> NotVerticalWhiteSpace)
                   <|> try (string "w" $> WordCharacter)
                   <|> try (string "W" $> NonWordCharacter)
                   <|> try (string "\\" $> BackslashChar)
                   <|> try (Nonalphanumeric <$> satisfy (not . isAlphaNum))
               )
        )

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

characterClassCharacters :: GenParser Char st CharacterClassCharacter
characterClassCharacters =
  try
    ( do
        a <- validChars
        _ <- string "-"
        b <- validChars
        case orderedRange a b of
          Just range -> return $ ClassRange range
          Nothing -> fail "Range out of order in charcter class"
    )
    <|> try (ClassLiteral <$> validChars)
    <|> try (QuotedClassLiterals <$> (string "\\Q" *> manyTill anyChar (try (string "\\E"))))
    <?> "CharacterClassCharacter"
  where
    validChars = noneOf "\\^[]-"
