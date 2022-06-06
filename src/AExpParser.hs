module AExpParser
  ( aExpr )
where

import           Text.ParserCombinators.Parsec

import           Lexer
import           Types

-- Arithmetische Ausdrücke werden durch folgende Grammatik gebildet.

-- AExpr → identifier
--       | integer
--       | AExpr Op AExpr
--       | `(` AExpr `)`

-- Op → `+`
--    | `-`
--    | `*`
--    | `/`
--    | `%`

-- Mit Vorrang der Operatoren:

-- Result -> AExpr

-- AExpr -> Term AExpr'

-- AExpr' -> + Term AExpr'
--        | - Term AExpr'
--        | ε

-- Term -> Factor Term'

-- Term' ->  * Factor Term'
--       | / Factor Term'
--       | % Factor Term'
--       | ε

-- Factor -> '(' AExpr ')'
--         | identifier
--         | integer

-- Dabei sind die üblichen Vorrangregeln von Operatoren wie "Punkt-vor-Strich"
-- zu berücksichtigen. D.h. `(1 + 2)*3` ist nicht gleich zu `1 + 2 * 3`.

-- | Der Parser für arithmetische Ausdrücke. Dies ist der Einstiegspunkt für das
-- Parsen für Ergebnisse des Typs `AExpr`

result :: Parser AExpr
result = 
  do 
    aExpr

aExpr :: Parser AExpr
aExpr = 
  do
    t <- term 
    expr <- aExpr'
    pure read ((show f : show t)::AExpr)
    
aExpr' :: Parser AExpr
aExpr' = 
  do
    op <- reservedOp "+"
    t <- term
    expr <- aExpr'
    pure (AExprPlus t expr)
  <|>
  do
    op <- reservedOp "-"
    t <- term
    expr <- aExpr'
    pure (AExprMinus t expr)
  <|>
  do
    e <- string ""
    pure (AExprVar (VarName e))

term :: Parser AExpr
term = 
  do 
    f <- factor 
    t <- term'
    pure read ((show f : show t)::AExpr)

term' :: Parser AExpr
term' =
  do
    op <- reservedOp "/"
    f <- factor 
    t <- term'
    pure (AExprDiv f t)
  <|>
  do
    op <- reservedOp "*"
    f <- factor 
    t <- term'
    pure (AExprMult f t)
  <|>
  do
    op <- reservedOp "%"
    f <- factor 
    t <- term'
    pure (AExprMod f t)
  <|>
  do
    e <- string ""
    pure (AExprVar (VarName e))

factor :: Parser AExpr
factor = 
  do
    _ <- char '('
    expr <- aExpr
    _ <- char ')'
    parenthesesAround (pure expr)
  <|>
  do 
    name <- identifier
    pure (AExprVar (VarName name))
  <|> 
  do 
    int <- integer
    pure (AExprInt int)


