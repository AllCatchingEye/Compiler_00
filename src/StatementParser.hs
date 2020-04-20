module StatementParser
  ( statement )
where

import           AExpParser (aExpr)
import           BExpParser (bExpr)
import           Lexer
import           Types

import           Text.ParserCombinators.Parsec


-- Statement → IfStatement `;`
--           | WhileStatement `;`
--           | SkipStatment `;`
--           | ReturnStatement `;`
--           | AssignStatement `;`
--           | Statement Statement

-- IfStatement → `if` `(` BExpr `)` `{` Statement `}` `else` `{` Statement `}`

-- WhileStatement → `while` `(` BExpr `)` `{` Statement `}`

-- SkipStatement → `skip`

-- ReturnStatement → `return` AExpr

-- AssignStatement → identifier `:=` AExpr

-- | Dieser Parser ist der Einstiegspunkt für Statements. Es wird entweder genau
-- ein Statement geparst oder eine Sequenz, also erst ein Statement und dann
-- rekursiv wieder ein `statement`.
statement :: Parser Statement
statement = error "statement parser not yet implemented"
