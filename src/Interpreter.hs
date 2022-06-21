module Interpreter
  ( interpret,
    interpretStatement,
    interpretStmt,
    interpretAexpr,
    interpretBexpr,
  )
where

import qualified Control.Monad as M (foldM)
import Types

-- | Diese Funktion interpretiert ein `Program` `p`. Dazu muss zuerst ein
-- initialer Programmzustand erzeugt werden, in dem ausgehend von
-- `emptyProgramState` alle Variablen von `p` deklariert werden.
--
-- Danach wird `interpretStatement` mit dem Programmzustand und dem `Statement`
-- aus `p` aufgerufen. Wenn danach der Wert von `result` im Prorgrammzustand
-- `Nothing` ist, dann soll ein `Left` Wert zurückgegeben werden, ansonsten ein
-- `Right` Wert, der das Ergebnis als `Integer` beinhaltet.
interpret :: Program -> Either String Integer
interpret p =
  case initialState of
    Just i -> case result (interpretStatement i (statements p)) of
      Nothing -> Left "did not see a result value"
      Just r -> Right r
    Nothing -> Left "cannot build initial program state"
  where
    initialState = M.foldM declareVar emptyProgramState (declaration p)

-- | Diese Funktion prüft ob das `result` des Programmzustands noch den Wert
-- `Nothing` hat. Wenn ja, wird das `Statement` s ausgeführt, ansonsten der
-- aktuelle Programmzustand `p` zurückgegeben.
interpretStatement :: ProgramState -> Statement -> ProgramState
interpretStatement ps@(ProgramState memory result) s =
  case result of
    Nothing -> interpretStmt ps s
    Just _ -> ps

-- | Diese Funktion macht eine Fallunterscheidung, je nachdem welches der
-- verschiedenen möglichen Statements übergeben wird. Zum Interpretieren eines
-- nächsten Statements wird `interpretStatement` aufgerufen, keine direkte
-- Rekursion.
interpretStmt :: ProgramState -> Statement -> ProgramState
interpretStmt ps@(ProgramState memory result) s =
  case s of
    StmtSkip -> ps
    StmtAssign vn ae ->
      case val of
        Nothing -> ps
        Just ps' -> ps'
      where
        val = updateVar ps vn (interpretAexpr ps ae)
    StmtIf be th el ->
      if bool then interpretStatement ps th else interpretStatement ps el
      where
        bool = interpretBexpr ps be
    StmtWhile be state ->
      if bool then interpretStatement ps state else ps
      where
        bool = interpretBexpr ps be
    StmtReturn ae -> 
      case val of -- Maybe Integer?
        Nothing -> ps
        Just val' -> ProgramState memory val'
      where
        val = interpretAexpr ps ae
    StmtSeq first next -> 
      case temp of -- Maybe ProgramState?
        Nothing -> ps
        Just ps' -> interpretStatement ps' next
      where 
        temp = interpretStatement ps first

-- | Diese Funktion interpretiert einen logischen Ausdruck.
interpretBexpr :: ProgramState -> BExpr -> Bool
interpretBexpr ps b = case b of
  BExprBool bool -> bool
  BExprNot be -> not (interpretBexpr ps be)
  BExprLT ae ae' -> interpretAexpr ps ae < interpretAexpr ps ae'
  BExprLTE ae ae' -> interpretAexpr ps ae <= interpretAexpr ps ae'
  BExprGT ae ae' -> interpretAexpr ps ae > interpretAexpr ps ae'
  BExprGTE ae ae' -> interpretAexpr ps ae >= interpretAexpr ps ae'
  BExprEq ae ae' -> interpretAexpr ps ae == interpretAexpr ps ae'
  BExprAnd be be' -> interpretBexpr ps be && interpretBexpr ps be'
  BExprOr be be' -> interpretBexpr ps be|| interpretBexpr ps be'
  BExprXor be be' -> interpretBexpr ps be /= interpretBexpr ps be'

-- | Diese Funktion interpretiert einen arithmetischen Ausdruck.
interpretAexpr :: ProgramState -> AExpr -> Integer
interpretAexpr ps a = case a of
  AExprInt n -> n
  AExprVar vn ->
    case val of
      Nothing -> 0 -- Korrekt?
      Just int -> int
    where
      val = varValue ps vn
  AExprPlus ae ae' -> interpretAexpr ps ae + interpretAexpr ps ae'
  AExprMinus ae ae' -> interpretAexpr ps ae - interpretAexpr ps ae'
  AExprMult ae ae' -> interpretAexpr ps ae * interpretAexpr ps ae'
  AExprDiv ae ae' -> interpretAexpr ps ae / interpretAexpr ps ae'
  AExprMod ae ae' -> interpretAexpr ps ae `mod` interpretAexpr ps ae'

