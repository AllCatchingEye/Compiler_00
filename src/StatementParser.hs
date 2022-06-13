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
  do
    stmnt <- ifStatement
    _ <- semi
    statement' stmnt
  <|>
  do
    stmnt <- whileStatement
    _ <- semi
    statement' stmnt
  <|>
  do
    stmnt <- skipStatement
    _ <- semi
    statement' stmnt
  <|>
  do
    stmnt <- returnStatement
    _ <- semi
    statement' stmnt
  <|>
  do
    stmnt <- assignStatement
    _ <- semi
    statement' stmnt

statement' :: Statement -> Parser Statement
statement' first =
  try (do
    sec <- statement
    next <- statement' sec
    pure StmtSeq first next)
  <|>
  do
    pure first

ifStatement :: Parser Statement
ifStatement = 
  do
    _ <- reserved "if"
    expr <- parenthesesAround bExpr
    stmt1 <- bracesAround statement
    _ <- reserved "else"
    stmt2 <- bracesAround statement
    pure StmtIf expr stmt1 stmt2

whileStatement :: Parser Statement
whileStatement = 
  do
    _ <- reserved "while"
    expr <- parenthesesAround bExpr
    stm <- bracesAround statement
    pure StmtWhile expr stm

skipStatement :: Parser Statement
skipStatement = 
  do
    _ <- reserved "skip"
    pure StmtSkip

returnStatement :: Parser Statement
returnStatement =
  do
    _ <- reserved "return"
    expr <- aExpr 
    pure StmtReturn expr

assignStatement :: Parser Statement
assignStatement =
  do
    name <- identifier
    _ <- reserved ":="
    expr <- aExpr
    pure StmtAssign name expr