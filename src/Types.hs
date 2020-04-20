module Types

where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map (empty, insert, lookup, member, notMember)

-- | Die interne Darstellung eines Programms. Diese Darstellung behinhaltet die
-- Liste der deklarierten Variablen sowie das geparste `Statement`. Da mehrere
-- Statements durch Sequenzstatements dargestellt werden, ist hier nur eines
-- gegeben.
data Program = Program
  { declaration :: [VarName]
  , statements  :: Statement
  } deriving (Show, Eq)

-- | Der Typ einer Programmvariable. Dieser besteht nur aus dem Namen als `String`.
data VarName = VarName
  { getVarName :: String
  }  deriving (Show, Eq, Ord)

-- | Ein arithmetischer Ausdruck. Dieser besteht aus einem der folgenden
-- Konstruktoren.
data AExpr = AExprInt Integer         -- ganzzahlige Konstante
           | AExprVar VarName         -- Variable
           | AExprPlus AExpr AExpr    -- Addition
           | AExprMinus AExpr AExpr   -- Subraktion
           | AExprMult AExpr AExpr    -- Multiplikation
           | AExprDiv AExpr AExpr     -- Division
           | AExprMod AExpr AExpr     -- Rest der Division
           deriving (Show, Eq)

-- | Ein logischer Ausdruck. Dieser besteht aus einem der folgenden
-- Konstruktoren.
data BExpr = BExprBool Bool           -- Boolesche Konstante
           | BExprNot BExpr           -- Negation
           | BExprLT AExpr AExpr      -- Vergleich `<`
           | BExprLTE AExpr AExpr     -- Vergleich `<=`
           | BExprGT AExpr AExpr      -- Vergleich `>`
           | BExprGTE AExpr AExpr     -- Vergleich `>=`
           | BExprEq AExpr AExpr      -- Vergleich `=`
           | BExprAnd BExpr BExpr     -- Konjunktion / UND
           | BExprOr BExpr BExpr      -- Disjunktion / ODER
           | BExprXor BExpr BExpr     -- exklusiv ODER
           deriving (Show, Eq)

-- | Ein Statement besteht aus den folgenden Möglichkeiten für Konstruktoren.
data Statement = StmtSkip                       -- skip / no-op
               | StmtAssign                     -- Zuweisung des Ergebnis der
                                                -- `AExpr` and die gegebene
                                                -- Variable
                  { lhs :: VarName              -- left hand side
                  , rhs :: AExpr}               -- right hand side
               | StmtSeq                        -- Sequenz zweier Statements
                  { first :: Statement
                  , next  :: Statement}
               | StmtIf                         -- Statement mit Bedingung
                  { ifCondition :: BExpr
                  , thenCase    :: Statement
                  , elsecase    :: Statement}
               | StmtWhile                      -- Schleife
                  { loopCondition :: BExpr
                  , loopBody      :: Statement}
               | StmtReturn                     -- Ergebnisrückgabe
                  { resultValue :: AExpr }
               deriving (Show, Eq)


-- | Der Programmzustand entspricht den momentanen Werten der deklarierten
-- Variablen zusammen mit einem optionalen Rückgabewert.
data ProgramState = ProgramState
  { getMemory :: Map VarName Integer
  , result    :: Maybe Integer}
  deriving (Show, Eq)

-- | Der `leere` Programmzustand als einfachster Zustand ohne deklarierte
-- Variablen und ohne Rückgabewert.
emptyProgramState :: ProgramState
emptyProgramState =
  ProgramState {
    getMemory = Map.empty
  , result = Nothing }

-- | Diese Funktion deklariert eine neue Variable im Programmzustand. Die
-- Rückgabe ist ein `Maybe ProgramState`. `Nothing`, wenn es die Variable schon
-- gab und `Just p` mit `ProgramState` `p` wenn es die Variable noch nicht
-- gab. Im zweiten Fall wird der Variable im Programmzustand der Startwert `0`
-- zugewiesen.
declareVar :: ProgramState -> VarName -> Maybe ProgramState
declareVar p@(ProgramState memory _) v
  | v `Map.member` memory = Nothing
  | otherwise             = Just (p { getMemory = Map.insert v 0 memory} )

-- | Diese Funktion ändert den Wert einer Variablen `v` zum Wert `val`. Der
-- Rückgabewert ist ein `Maybe ProgramState`. Der Wert ist `Nothing` wenn die
-- Variable noch nicht existiert und `Just p` mit `ProgramState` `p` wobei der
-- bisherige Wert von `v` mit `val` ersetzt ist.
updateVar :: ProgramState -> VarName -> Integer -> Maybe ProgramState
updateVar p@(ProgramState memory _) var val
  | var `Map.notMember` memory = Nothing
  | otherwise                  = Just (p { getMemory = Map.insert var val memory })

-- | Diese Funktion liefert den aktuellen Wert der Variable `v`. Der
-- Rückgabewert ist ein `Maybe Integer`. `Nothing` wenn die Variable nicht im
-- Programmzustand existiert und `Just i` mit `Integer` `i` wenn die Variable
-- existiert.
varValue :: ProgramState -> VarName -> Maybe Integer
varValue (ProgramState memory _) var = Map.lookup var memory
