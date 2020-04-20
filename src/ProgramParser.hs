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
program = error "program parser not yet implemented"
