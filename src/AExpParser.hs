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

-- Dabei sind die üblichen Vorrangregeln von Operatoren wie "Punkt-vor-Strich"
-- zu berücksichtigen. D.h. `(1 + 2)*3` ist nicht gleich zu `1 + 2 * 3`.

-- | Der Parser für arithmetische Ausdrücke. Dies ist der Einstiegspunkt für das
-- Parsen für Ergebnisse des Typs `AExpr`
aExpr :: Parser AExpr
aExpr = error "artimethic expression parser not yet implemented"
