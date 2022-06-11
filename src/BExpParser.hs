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
    pure (BExprOr t1 (bExpr' t2))
  <|>
  do
    bOP <- reservedOp "^"
    t2 <- term
    pure (BExprXor t1 (bExpr' t2))
  do 
    pure t1

term :: Parser BExpr
term = 
  do
    b <- boolean
    term' b

term' :: Parser BExpr
term' b1 = 
  do 
    bOP <- reservedOp "&&"
    b2 <- boolean
    pure (BExprAnd b1 (term' b2))

-- TODO
boolean :: Parser BExpr
boolean =
  do
    tt <- string "tt"
    pure (BExprBool True)
  <|>
  do
    ff <- string "ff"
    pure (BExprBool False)
  <|>
  do
    inv <- string "not"
    expr <- bExpr
    pure (BExprNot expr)
  <|>
  do
    _ <- char '('
    expr <- bExpr
    _ <- char ')'
    parenthesesAround expr
  <|>
  do
    expr <- cmpExpr
    pure expr

cmpExpr :: Parser BExpr
cmpExpr = 
  try do
    op <- reservedOp "<="
    pure (BExprLTE aExpr aExpr)
  <|>
  do
    op <- reservedOp "<"
    pure (BExprLT aExpr aExpr)
  <|>
  try do
    op <- reservedOp ">="
    pure (BExprGTE aExpr aExpr)
  <|>
  do
    op <- reservedOp ">"
    pure (BExprGT aExpr aExpr)
  <|>
  do
    op <- reservedOp "="
    pure (BExprEq aExpr aExpr)
