{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with fromMaybe" #-}
module Interpreter
  ( interpret,
    interpretCount,
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

interpretCount :: Program -> Either String (Integer, Integer)
interpretCount p =
  case initialState of
    Just i -> case result (interpretStatement i (statements p)) of
      Nothing -> Left "did not see a result value"
      Just r -> Right (r, counter i)
    Nothing -> Left "cannot build initial program state"
  where
    initialState = M.foldM declareVar emptyProgramState (declaration p)

-- | Diese Funktion prüft ob das `result` des Programmzustands noch den Wert
-- `Nothing` hat. Wenn ja, wird das `Statement` s ausgeführt, ansonsten der
-- aktuelle Programmzustand `p` zurückgegeben.
interpretStatement :: ProgramState -> Statement -> ProgramState
interpretStatement ps@(ProgramState memory result counter) s =
  case result of
    Nothing -> interpretStmt new_ps s
    Just _ -> new_ps
  where
    new_ps = ProgramState memory result (counter + 1)

-- | Diese Funktion macht eine Fallunterscheidung, je nachdem welches der
-- verschiedenen möglichen Statements übergeben wird. Zum Interpretieren eines
-- nächsten Statements wird `interpretStatement` aufgerufen, keine direkte
-- Rekursion.
interpretStmt :: ProgramState -> Statement -> ProgramState
interpretStmt ps@(ProgramState memory result counter) s =
  case s of
    StmtSkip -> new_ps
    StmtAssign vn ae ->
      case val of
        Nothing -> error "Coudn't assign value"
        Just ps' -> ps'
      where
        val = updateVar new_ps vn (interpretAexpr new_ps ae)
    StmtIf be th el ->
      if bool then interpretStatement new_ps th else interpretStatement new_ps el
      where
        bool = interpretBexpr new_ps be
    StmtWhile be state ->
      if bool then interpretStatement (interpretStatement new_ps state) (StmtWhile be state) else new_ps
      where
        bool = interpretBexpr new_ps be
    StmtReturn ae -> ProgramState (getMemory new_ps) (Just (interpretAexpr new_ps ae)) (counter + 1)
    StmtSeq first next -> interpretStatement ps' next
      where
        ps' = interpretStatement new_ps first
  where
    new_ps = ProgramState memory result (counter + 1)

-- | Diese Funktion interpretiert einen logischen Ausdruck.
interpretBexpr :: ProgramState -> BExpr -> Bool
interpretBexpr ps@(ProgramState memory result counter) b = case b of
  BExprBool bool -> bool
  BExprNot be -> not (interpretBexpr new_ps be)
  BExprLT ae ae' -> interpretAexpr new_ps ae < interpretAexpr new_ps ae'
  BExprLTE ae ae' -> interpretAexpr new_ps ae <= interpretAexpr new_ps ae'
  BExprGT ae ae' -> interpretAexpr new_ps ae > interpretAexpr new_ps ae'
  BExprGTE ae ae' -> interpretAexpr new_ps ae >= interpretAexpr new_ps ae'
  BExprEq ae ae' -> interpretAexpr new_ps ae == interpretAexpr new_ps ae'
  BExprAnd be be' -> interpretBexpr new_ps be && interpretBexpr new_ps be'
  BExprOr be be' -> interpretBexpr new_ps be || interpretBexpr new_ps be'
  BExprXor be be' -> interpretBexpr new_ps be /= interpretBexpr new_ps be'
  where
    new_ps = ProgramState memory result (counter + 1)

-- | Diese Funktion interpretiert einen arithmetischen Ausdruck.
interpretAexpr :: ProgramState -> AExpr -> Integer
interpretAexpr ps@(ProgramState memory result counter) a = case a of
  AExprInt n -> n
  AExprVar vn ->
    case val of
      Nothing -> error ("Variable " ++ show vn ++ "not found")
      Just int -> int
    where
      val = varValue new_ps vn
  AExprPlus ae ae' -> interpretAexpr new_ps ae + interpretAexpr new_ps ae'
  AExprMinus ae ae' ->
    if negativ (interpretAexpr ps ae')
      then interpretAexpr new_ps ae + interpretAexpr new_ps ae' * (-1)
      else interpretAexpr new_ps ae - interpretAexpr new_ps ae'
  AExprDiv ae ae' -> interpretAexpr new_ps ae `div` interpretAexpr new_ps ae'
  AExprMult ae ae' -> interpretAexpr new_ps ae * interpretAexpr new_ps ae'
  AExprMod ae ae' -> interpretAexpr new_ps ae `mod` interpretAexpr new_ps ae'
  where
    new_ps = ProgramState memory result (counter + 1)

negativ :: Integer -> Bool
negativ i = i < 0
