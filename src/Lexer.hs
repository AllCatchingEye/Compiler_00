module Lexer

where

import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Tok

-- Parsec nutzt nicht unbedingt einen expliziten Lexer / Scanner. Man kann
-- allerdings einen speziellen Parser für reservierte Wörter, reservierte
-- Operatoren und Bezeichner definieren.
--
-- `Tok.identStart` soll ein Groß- oder Kleinbuchstabe sein
--
-- `Tok.identLetter` soll ein(!) Groß- oder Kleinbuchstabe oder eine Ziffer sein
--
-- `Tok.reservedNames` ist eine Liste von Strings von keywords, d.h. also die
-- Worte, die im Programm vorkommen können aber keine Bezeichner sind. In Java
-- z.B. "new", "for", "class" und "private"
--
-- `Tok.reservedOpNames` ist eine Liste von Strings von Operatoren, die im
-- Programm vorkommen können.
--
-- Die reservierten Wörten können dann z.B. mit `reserved "foo"` geparst werden,
-- wenn "foo" in der Liste `Tok.reservedNames` vorkommt.
--
-- Analog dazu kann mit `reservedOp "::"` z.B. ein Operator `::` geparst werden,
-- wenn dieser in der Liste `Tok.reservedOpNames` vorkommt.
--
-- Der Vorteil der Verwendung dieses Tokenparsers ist, dass jeweils auch
-- nachfolgende Leerzeichen als sog. Whitespace entfernt werden.
--
-- d.h. wenn z.B. der Operator `::` geparst werden soll, schreiben Sie nicht
-- `string "::"` sondern `reservedOp "::"` und anstelle von z.B. `string "new"`
-- für das keyword "new" schreiben Sie `reserved "new"`
langDef :: LanguageDef u
langDef = emptyDef {
    Tok.identStart = 
      do
        oneOf ['a'..'z']
      <|>
      do
        oneOf ['A'..'Z']         
                                        -- definieren Sie hier einen Parser für
                                        -- _das_ erste Zeichen eines Bezeichners

  , Tok.identLetter = 
      do
        oneOf ['a'..'z']
        <|>
      do
        oneOf ['A'..'Z'] 
      <|>
      do
        oneOf ['0'..'9']
                                        -- definieren Sie hier einen Parser für
                                        -- _ein_ valides Zeichen eines
                                        -- Bezeichners, das nicht das erste
                                        -- Zeichen ist

  , Tok.reservedNames = ["decl", "if", "else", "while", "return",
                         "skip",  "then", "tt", "ff", "not"]       
                                        -- definieren Sie hier eine Liste von
                                        -- keywords als strings

  , Tok.reservedOpNames = ["+", "-", "*", "/", "%",
                           "||", "&&", "^", 
                           ">", "<=", ">", ">=", "=",
                           ":="]     
                                        -- definieren Sie hier eine Liste von
                                        -- Operatoren als strings
  }


-- | `lexer` wird hier der Parsec-interne `Tokenparser`
lexer :: Tok.TokenParser t
lexer = Tok.makeTokenParser langDef


-- | Die folgengen Parser werden aus dem Tokenparser oben generiert und Sie
-- können / sollten diese in Ihren Parsern verwenden.

-- | `identifier` kann Bezeichner parsen
identifier :: Parser String
identifier = Tok.identifier lexer

-- | `parenthesesAround` nimmt einen Parser `p` und parst `(` p `)`
parenthesesAround :: Parser a -> Parser a
parenthesesAround = Tok.parens lexer

-- | `bracesAround` nimmt einen Parser `p` und parst `{` p `}`
bracesAround :: Parser a -> Parser a
bracesAround = Tok.braces lexer

-- | `reserved w` parst das reservierte Wort `w`
reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- | `reservedOp op` parst den reservierten Operator `op`
reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp lexer

-- | `integer` parst einen Integer (auch negativ)
integer :: Parser Integer
integer = Tok.integer lexer

-- | `semi` parst einen Strichpunkt
semi :: Parser String
semi = Tok.semi lexer

-- | `whitespace` parst sogenannten Whitespace, also Leerzeichen, Tabulatoren
-- und Newlines, die man i.A. ignorieren will. Die oben generierten Parser
-- konsumieren allen whitespace, der _nach_ einem Token kommt, nicht whitespace
-- davor.
whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer
