module BExpParser (bExpr) where

import AExpParser (aExpr)
import Lexer
import Text.ParserCombinators.Parsec
import Types

-- Boolesch / logische Ausdrücke werden durch folgende Grammatik gebildet

-- BExpr → `tt`
--       | `ff`
--       | `not` BExpr
--       | BExpr BOp BExpr
--       | AExpr CmpOp AExpr
--       | `(` BExpr `)`

-- BExpr → `tt`
--       | `ff`
--       | `not` BExpr

-- Nach entfernen der mehrdeutigkeit

-- BExpr -> Term BExpr'

-- BExpr' -> || Term BExpr'
--         | ^ Term BExpr'
--         | ε

-- Term -> Boolean Term'

-- Term' -> && Boolean Term'
--        | ε

-- Boolean -> tt
--         | ff
--         | not BExpr
--         | cmpExpr
--         | `(` BExpr `)`

-- cmpExpr -> AExpr > AExpr
--         | AExpr < AExpr
--         | AExpr >= AExpr
--         | AExpr <= AExpr
--         | AExpr = AExpr

-- Dabei ist `tt` die Konstante für `True`, `ff` für `False`. `not` ist die
-- Negation. `&&` ist das logische AND, `||` das logische OR und `^` das
-- logische XOR. Analog wie bei arithmetischen Ausdrücken werden die geowhnten
-- Regeln für den Vorrang beachtet, d.h. erst `&&`, dann `||`/`^`.

-- | Dieser Parser ist der Einstiegspunkt für das Parsen von logischen Ausdrücken.
-- Der Parser sucht zuerst nach && Ausdrücken, die Vorrangig geparst werden.
-- Erst danach werden || und ^ geparst.
bExpr :: Parser BExpr
bExpr =
  do
    _ <- whiteSpace
    t <- term
    bExpr' t

-- Parst || und ^. Wird keiner der Operatoren gefunden,
-- wird die übergebene BExpr zurückgegeben.
bExpr' :: BExpr -> Parser BExpr
bExpr' t1 =
  do
    _ <- reservedOp "||"
    t2 <- term
    bExpr' (BExprOr t1 t2)
    <|> do
      _ <- reservedOp "^"
      t2 <- term
      bExpr' (BExprXor t1 t2)
    <|> do
      pure t1

-- Einstiegspunkt um booleans und Ausdrücke mit && zu parsen.
term :: Parser BExpr
term =
  do
    b <- boolean
    term' b

-- Parst BExpr die ein && enthält.
term' :: BExpr -> Parser BExpr
term' b1 =
  do
    _ <- reservedOp "&&"
    b2 <- boolean
    term' (BExprAnd b1 b2)
    <|> do
      pure b1

-- Parst eine BExpr und gibt einen Boolschen Ausdruck zurück.
-- Beispiele:
-- tt -> BExprBool True
-- ff -> BExprBool False
-- not BExpr -> BExprNot BExpr
-- BExpr -> (BExpr)
-- Wird eine BExpr mit Vergleichsoperatoren gefunden, wird ein
-- eigener Parser namens cmpExpr dafür aufgerufen.
boolean :: Parser BExpr
boolean =
  try
    ( do
        _ <- string "tt"
        _ <- whiteSpace
        pure (BExprBool True)
    )
    <|> try
      ( do
          _ <- string "ff"
          _ <- whiteSpace
          pure (BExprBool False)
      )
    <|> try
      ( do
          _ <- string "not"
          expr <- bExpr
          pure (BExprNot expr)
      )
    <|> try
      ( do
          expr <- parenthesesAround bExpr
          pure expr
      )
    <|> do
      expr <- cmpExpr
      pure expr

-- Parser der BExpr mit Vergleichsoperatoren parst.
-- Beispiele:
-- 5 > 3 -> BExprGT 5 3
-- 5 >= 5 -> BExprGTE 5 5
-- 5 < 3 -> BExprLT 5 3
-- 5 <= 5 -> BExprLTE 5 5
-- 5 = 5 -> BExprEq 5 5
cmpExpr :: Parser BExpr
cmpExpr =
  try
    ( do
        expr1 <- aExpr
        _ <- reservedOp ">="
        expr2 <- aExpr
        pure (BExprGTE expr1 expr2)
    )
    <|> try
      ( do
          expr1 <- aExpr
          _ <- reservedOp ">"
          expr2 <- aExpr
          pure (BExprGT expr1 expr2)
      )
    <|> try
      ( do
          expr1 <- aExpr
          _ <- reservedOp "<="
          expr2 <- aExpr
          pure (BExprLTE expr1 expr2)
      )
    <|> try
      ( do
          expr1 <- aExpr
          _ <- reservedOp "<"
          expr2 <- aExpr
          pure (BExprLT expr1 expr2)
      )
    <|> do
      expr1 <- aExpr
      _ <- reservedOp "="
      expr2 <- aExpr
      pure (BExprEq expr1 expr2)
