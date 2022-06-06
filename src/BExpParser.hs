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

-- Return -> BExpr

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

return :: Parser BExpr
return = do bExpr

bExpr :: Parser BExpr
bExpr = 
  do
    t <- term
    epxr <- BExpr'
    pure (t : expr)

bExpr' :: Parser BExpr
bExpr' = 
  do 
    bOP  <- reservedOp "||"
    t <- term
    expr <- bExpr'
    pure (BExprOr t expr)
  <|>
  do
    bOP <- reservedOp "^"
    t <- term
    expr <- bExpr'
    pure (BExprXor t expr)
  do 
    _ <- bExpr'
    pure (AExprVar (VarName ""))

term :: Parser BExpr
term = 
  do
    b <- boolean
    t <- term'
    pure (b : t)

term' :: Parser BExpr
term' = 
  do 
    bOP <- reservedOp "&&"
    b <- boolean
    t <- term'
    pure (BExprAnd b t)

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
    expr <- cmpExpr
    pure expr

cmpExpr :: Parser BExpr
cmpExpr = 
  try do
    do
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
