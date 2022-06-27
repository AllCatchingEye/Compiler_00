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
-- Parsen für Ergebnisse des Typs `AExpr`. Anfängliche Whitespaces werden ignoriert,
-- Vorrang von *, / und % wird hierbei beachtet, + und - werden erst zum schluß geparst.
aExpr :: Parser AExpr
aExpr =
  do
    _ <- whiteSpace
    t <- term
    aExpr' t

-- Parser der + und - parst. Nach einem Rechenoperator kann ein Term geparst werden,
-- welcher *, / und % parsen kann. Wenn weder + noch - engelesen werden, wird die übergebene AExpr zurückgegeben.
-- Beispiele:
-- 5 + 3 -> AExprPlus 5 3
-- 5 - 3 -> AExprMinus 5 3
-- 42 -> 42
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

-- Einstiegspunkt um *, / und % zu parsen.
term :: Parser AExpr
term =
  do
    f <- factor
    term' f

-- Parst *, / und %. Wird keiner der Operatoren erkannt,
-- wird die übergebene AExpr wieder zurückgegeben
-- Beispiele:
-- 5 * 3 -> AExprMult 5 3
-- 5 / 3 -> AExprDiv 5 3
-- 5 % 3 -> AExprMod 5 3
-- 42 -> 42
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

-- Parst einen Faktor. Ein Faktor kann eine Variable, Nummer und geklammerte AExpr sein.
-- Beispiele:
-- a -> AExprVar (Varname a)
-- 5 -> AExprInt 5
-- AExpr -> (AExpr)
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
