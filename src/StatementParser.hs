module StatementParser (statement) where

import AExpParser (aExpr)
import BExpParser (bExpr)
import Lexer
import Text.ParserCombinators.Parsec
import Types

-- Statement → IfStatement `;`
--           | WhileStatement `;`
--           | SkipStatment `;`
--           | ReturnStatement `;`
--           | AssignStatement `;`
--           | Statement Statement

-- Nach entfernen der Linksrekursion:

-- Statement -> IfStatement ';' Statement'
--            | WhileStatement `;` Statement'
--            | SkipStatment `;` Statement'
--            | ReturnStatement `;` Statement'
--            | AssignStatement `;` Statement'

-- Statement' -> Statement Statement'
--            | ε

-- IfStatement → `if` `(` BExpr `)` `{` Statement `}` `else` `{` Statement `}`

-- WhileStatement → `while` `(` BExpr `)` `{` Statement `}`

-- SkipStatement → `skip`

-- ReturnStatement → `return` AExpr

-- AssignStatement → identifier `:=` AExpr

-- | Dieser Parser ist der Einstiegspunkt für Statements. Es wird entweder genau
-- ein Statement geparst oder eine Sequenz, also erst ein Statement und dann
-- rekursiv wieder ein `statement`.
statement :: Parser Statement
statement =
  try
    ( do
        _ <- whiteSpace
        stmnt <- ifStatement
        _ <- semi
        statement' stmnt
    )
    <|> try
      ( do
          _ <- whiteSpace
          stmnt <- whileStatement
          _ <- semi
          statement' stmnt
      )
    <|> try
      ( do
          _ <- whiteSpace
          stmnt <- skipStatement
          _ <- semi
          statement' stmnt
      )
    <|> try
      ( do
          _ <- whiteSpace
          stmnt <- returnStatement
          _ <- semi
          statement' stmnt
      )
    <|> do
      _ <- whiteSpace
      stmnt <- assignStatement
      _ <- semi
      statement' stmnt

-- Parst ein Statement. Das Statement kann aus mehreren Statements bestehen.
statement' :: Statement -> Parser Statement
statement' first =
  try
    ( do
        sec <- statement
        next <- statement' sec
        pure StmtSeq {first = first, next = next}
    )
    <|> do
      pure first

-- Parst ein if Statement. Das if Statement besteht aus einer
-- BExpr, dem Statement im then Block und dem Statement im else Block.
-- Beispiel:
-- If(BExpr){ a = 5 }else{ a = 0 } -> StmtIf BExpr (a = 5) (a = 0)
ifStatement :: Parser Statement
ifStatement =
  do
    _ <- whiteSpace
    _ <- reserved "if"
    expr <- parenthesesAround bExpr
    stmt1 <- bracesAround statement
    _ <- reserved "else"
    stmt2 <- bracesAround statement
    pure StmtIf {ifCondition = expr, thenCase = stmt1, elsecase = stmt2}

-- Parst eine While Statement. Das While Statement besteht aus einer
-- BExpr, und dem Statement im Body
-- Beispiel:
-- while(BExpr){ a = a + 1} -> StmtWhile BExpr (a = a + 1)
whileStatement :: Parser Statement
whileStatement =
  do
    _ <- whiteSpace
    _ <- reserved "while"
    expr <- parenthesesAround bExpr
    stm <- bracesAround statement
    pure StmtWhile {loopCondition = expr, loopBody = stm}

-- Ein parser der Skip parst.
-- Beispiel: skip -> StmtSkip
skipStatement :: Parser Statement
skipStatement =
  do
    _ <- whiteSpace
    _ <- reserved "skip"
    pure StmtSkip

-- Ein Parser der ein Return Statement parst.
-- Beispiel: return a -> StmtReturn a
returnStatement :: Parser Statement
returnStatement =
  do
    _ <- whiteSpace
    _ <- reserved "return"
    expr <- aExpr
    pure StmtReturn {resultValue = expr}

-- Ein Parser der ein Assign Statement parst.
-- Beispiel: a = a + 1 -> Stmt Assign (Varname a) (a + 1)
assignStatement :: Parser Statement
assignStatement =
  do
    _ <- whiteSpace
    name <- identifier
    _ <- reservedOp ":="
    expr <- aExpr
    pure StmtAssign {lhs = VarName name, rhs = expr}