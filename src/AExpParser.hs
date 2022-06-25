module AExpParser (aExpr) where

import Lexer
import Text.ParserCombinators.Parsec
import Types

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

-- Mit Vorrang der Operatoren und ohne Linkrekursion:

-- AExpr -> Term AExpr'

-- AExpr' -> + Term AExpr'
-- AExpr'  | - Term AExpr'
--         | ε

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
aExpr :: Parser AExpr
aExpr =
  do
    _ <- whiteSpace
    t <- term
    aExpr' t

aExpr' :: AExpr -> Parser AExpr
aExpr' t1 =
  do
    _ <- reservedOp "+"
    t2 <- term
    aExpr' (AExprPlus t1 t2)
    <|> do
      _ <- reservedOp "-"
      t2 <- term
      aExpr' (AExprMinus t1 t2)
    <|> do
      pure t1

term :: Parser AExpr
term =
  do
    f <- factor
    term' f

term' :: AExpr -> Parser AExpr
term' f1 =
  do
    _ <- reservedOp "*"
    f2 <- factor
    term' (AExprMult f1 f2)
    <|> do
      _ <- reservedOp "/"
      f2 <- factor
      term' (AExprDiv f1 f2)
    <|> do
      _ <- reservedOp "%"
      f2 <- factor
      term' (AExprMod f1 f2)
    <|> do
      pure f1

factor :: Parser AExpr
factor =
  do
    expr <- parenthesesAround aExpr
    pure expr
    <|> do
      name <- identifier
      pure (AExprVar (VarName name))
    <|> do
      int <- integer
      pure (AExprInt int)
