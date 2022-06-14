module BExpParser
  ( bExpr )
where

import           Text.ParserCombinators.Parsec

import           AExpParser (aExpr)
import           Lexer
import           Types

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

bExpr :: Parser BExpr
bExpr = 
  do
    t <- term
    bExpr' t

bExpr' :: BExpr -> Parser BExpr
bExpr' t1 = 
  do 
    bOP  <- reservedOp "||"
    t2 <- term
    expr <- bExpr' t2
    pure (BExprOr t1 expr)
  <|>
  do
    bOP <- reservedOp "^"
    t2 <- term
    expr <- bExpr' t2
    pure (BExprXor t1 expr)
  <|>
  do 
    pure t1

term :: Parser BExpr
term = 
  do
    b <- boolean
    term' b

term' :: BExpr -> Parser BExpr
term' b1 = 
  do 
    bOP <- reservedOp "&&"
    b2 <- boolean
    expr <- term' b2
    pure (BExprAnd b1 expr)
  <|>
  do
    pure b1

-- TODO
boolean :: Parser BExpr
boolean =
  do
    tt <- string "tt"
    _ <- whiteSpace
    pure (BExprBool True)
  <|>
  do
    ff <- string "ff"
    _ <- whiteSpace
    pure (BExprBool False)
  <|>
  do
    inv <- string "not"
    expr <- bExpr
    pure (BExprNot expr)
  <|>
  do
    expr <- parenthesesAround bExpr
    pure expr
  <|>
  do
    expr <- cmpExpr
    pure expr

cmpExpr :: Parser BExpr
cmpExpr = 
  try (do
    expr1 <- aExpr
    _ <- reservedOp "<="
    expr2 <- aExpr
    pure (BExprLTE expr1 expr2))
  <|>
  do
    expr1 <- aExpr
    _ <- reservedOp "<"
    expr2 <- aExpr
    pure (BExprLT expr1 expr2)
  <|>
  try (do
    expr1 <- aExpr
    _ <- reservedOp ">="
    expr2 <- aExpr
    pure (BExprGTE expr1 expr2))
  <|>
  do
    expr1 <- aExpr
    _ <- reservedOp ">"
    expr2 <- aExpr
    pure (BExprGT expr1 expr2)
  <|>
  do
    expr1 <- aExpr
    _ <- reservedOp "="
    expr2 <- aExpr
    pure (BExprEq expr1 expr2)
