module Interpreter
  ( interpret
  , interpretStatement
  , interpretStmt
  , interpretAexpr
  , interpretBexpr)
where

import qualified Control.Monad as M (foldM)
import           Types

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
    Just i  -> case result (interpretStatement i (statements p)) of
                   Nothing -> Left "did not see a result value"
                   Just r  -> Right r
    Nothing -> Left "cannot build initial program state"
  where initialState = M.foldM declareVar emptyProgramState (declaration p)
        -- ^^ wendet `declareVar` beginnend mit dem `emptyProgramState` auf alle
        --    deklarierten Variablen an und sammelt das Ergebnis in einem `Maybe
        --    ProgramState` Typ

-- | Diese Funktion prüft ob das `result` des Programmzustands noch den Wert
-- `Nothing` hat. Wenn ja, wird das `Statement` s ausgeführt, ansonsten der
-- aktuelle Programmzustand `p` zurückgegeben.
interpretStatement :: ProgramState -> Statement -> ProgramState
interpretStatement p s =
  case result p of
    Nothing -> interpretStmt p s
    Just _  -> p

-- | Diese Funktion macht eine Fallunterscheidung, je nachdem welches der
-- verschiedenen möglichen Statements übergeben wird. Zum Interpretieren eines
-- nächsten Statements wird `interpretStatement` aufgerufen, keine direkte
-- Rekursion.
interpretStmt :: ProgramState -> Statement -> ProgramState
interpretStmt = error "interpreter for statement not yet implemented"


-- | Diese Funktion interpretiert einen logischen Ausdruck.
interpretBexpr :: ProgramState -> BExpr -> Bool
interpretBexpr = error "interpreter for Boolean expression not yet implemented"

-- | Diese Funktion interpretiert einen arithmetischen Ausdruck.
interpretAexpr :: ProgramState -> AExpr -> Integer
interpretAexpr = error "interpreter for arithmetic expression not yet imeplemnted"
