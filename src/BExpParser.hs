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
-- BOp → `&&`
--     | `||`
--     | `^`

-- Dabei ist `tt` die Konstante für `True`, `ff` für `False`. `not` ist die
-- Negation. `&&` ist das logische AND, `||` das logische OR und `^` das
-- logische XOR. Analog wie bei arithmetischen Ausdrücken werden die geowhnten
-- Regeln für den Vorrang beachtet, d.h. erst `&&`, dann `||`/`^`.

-- | Dieser Parser ist der Einstiegspunkt für das Parsen von logischen Ausdrücken.
bExpr :: Parser BExpr
bExpr = error "Boolean expression parser not yet implemented"
