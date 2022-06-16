module ProgramParser
  ( program
  , parseProgram)
where

import           Lexer
import           StatementParser (statement)
import           Types

import           Text.ParserCombinators.Parsec

-- | Diese Funktion ist der Einstiegspunkt. Es wird versucht, den übergebenen
-- `String` `s` als `Program` zu parsen. Wenn das erfolgreich ist, wird ein Wert
-- `Right` mit dem `Program` zurückgegeben, ansonsten ein Wert `Left` mit einer
-- Fehlermeldung.
parseProgram :: String -> Either ParseError Program
parseProgram s = parse (program <* eof) "" s

-- | Diese Funktion ist der Parser für ein `Program` und dient sozusagen als
-- Startsymbol der Grammatik.
program :: Parser Program
program = 
  do
    _ <- whiteSpace
    do
      _ <- reserved "decl"
      variables <- many (try (do
                  name <- identifier
                  _ <- optionMaybe (char ',')
                  _ <- whiteSpace
                  pure (VarName name)))
      _ <- semi
      stmt <- statement
      pure Program {declaration = variables, statements = stmt}
    <|>
    do
      stmt <- statement
      pure Program {declaration = [], statements = stmt}


   
