module StatementParserSpec
  ( spec
  , parseSuccess
  , exAssign)
where

import           Test.Hspec

import           StatementParser
import           Types
import           Util

exAssign :: TestPairs Statement
exAssign =
  [ ( "a := 3;"
    , Just $ StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 3})
  , ( "a:= -3;"
    , Just $ StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt (-3)})
  , ( "a:=  b;"
    , Just $ StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprVar (VarName {getVarName = "b"})})
  , ( "a:=b;"
    , Just $ StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprVar (VarName {getVarName = "b"})})
  , ( "a     :=    b  ;   "
    , Just $ StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprVar (VarName {getVarName = "b"})})]

-- negative Beispiele
negExAssign :: TestPairs Statement
negExAssign = [ ("a:=1", Nothing)
              , ("a:=b", Nothing)
              , ("a:=-1", Nothing)
              , ("a:=", Nothing)
              , ("a:=;", Nothing)
              , ("a=1;", Nothing)
              ]
------------
-- return --
------------

exReturn :: TestPairs Statement
exReturn =
  [ ( "return a;"
    , Just $ StmtReturn {resultValue = AExprVar (VarName {getVarName = "a"})})
  , ( "return (a);"
    , Just $ StmtReturn {resultValue = AExprVar (VarName {getVarName = "a"})})
  , ( "return -1;"
    , Just $ StmtReturn {resultValue = AExprInt (-1)})
  ]

negExReturn :: TestPairs Statement
negExReturn = [ ("return   ", Nothing)
              , ("return", Nothing)
              , ("return;", Nothing)
              , ("return a", Nothing)
              ]

----------
-- skip --
----------

exSkip :: TestPairs Statement
exSkip = [ ("skip;", Just StmtSkip)
         , ("skip  ;  ", Just StmtSkip)
         ]

negExSkip :: TestPairs Statement
negExSkip = [ ("skip", Nothing)
            , ("skip  ", Nothing)
            , ("skip 1;", Nothing)
            ]

--------
-- if --
--------

exIf :: TestPairs Statement
exIf =
  [ ( "if (tt) { skip; } else { skip; };"
    , Just (StmtIf {ifCondition = BExprBool True, thenCase = StmtSkip, elsecase = StmtSkip}))
  , ( "if (tt) { a:= 2; } else { b := -2; };"
    , Just (StmtIf {ifCondition = BExprBool True, thenCase = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 2}, elsecase = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt (-2)}}))
  , ( "if (tt) { a:= 2; skip; } else { b := -2; a:= b; };"
    , Just (StmtIf {ifCondition = BExprBool True, thenCase = StmtSeq {first = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 2}, next = StmtSkip}, elsecase = StmtSeq {first = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt (-2)}, next = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprVar (VarName {getVarName = "b"})}}})
    )
  ]

negExIf :: TestPairs Statement
negExIf =
  [ ( "if (tt) {  } else { b := -2; a:= b; };", Nothing)
  , ( "if {  } else { b := -2; a:= b; };", Nothing)
  , ( "if (tt){ skip; } else {  };", Nothing)
  , ( "if (tt){ skip; };", Nothing)
  ]

exWhile :: TestPairs Statement
exWhile =
  [ ( "while  (ff) { skip; }  ;  "
    , Just (StmtWhile {loopCondition = BExprBool False, loopBody = StmtSkip}))
  , ( "while  (ff) { a:=   b; b:= -2; }; "
    , Just (StmtWhile {loopCondition = BExprBool False, loopBody = StmtSeq {first = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprVar (VarName {getVarName = "b"})}, next = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt (-2)}}}))
  ]

negExWhile :: TestPairs Statement
negExWhile = [ ("while  (ff) {  }; ", Nothing)
             , ("while  () { a:=   b; b:= -2; }; ", Nothing)
             , ("while  (ff) { a:=   b; b:= -2; } ", Nothing)
             ]

--------------
-- sequence --
--------------

exSeq :: TestPairs Statement
exSeq =
  [ ( "a :=1; b:=2;"
    , Just (StmtSeq {first = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 1}, next = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt 2}}))
  , ( "a :=    1;\n   b :=2;"
    , Just (StmtSeq {first = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 1}, next = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt 2}}))
  , ( "a :=1; return 1;"
    , Just (StmtSeq {first = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 1}, next = StmtReturn {resultValue = AExprInt 1}}))
  , ( "return -1;\n   b :=2;"
    , Just (StmtSeq {first = StmtReturn {resultValue = AExprInt (-1)}, next = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt 2}}))
  ]

negExSeq :: TestPairs Statement
negExSeq = [ ("a:= 1\nb:=1; ", Nothing)
           , ("a:= 1;\nb:=1", Nothing)
           ]

spec :: Spec
spec = do
  describe "Parse assign statements" $ do

    it "valid assign statements" $
      checkStatementExamples statement exAssign
    it "invalid assignments" $
      checkStatementExamples statement negExAssign

  describe "Parse return statements" $ do

    it "valid return statements" $
      checkStatementExamples statement exReturn
    it "invalid return statements" $
      checkStatementExamples statement negExReturn

  describe "parse skip statements" $ do
    it "valid skip statements" $
      checkStatementExamples statement exSkip
    it "invalid skip statements" $
      checkStatementExamples statement negExSkip


  describe "conditional if" $ do
    it "valid if statments" $
      checkStatementExamples statement exIf
    it "invalid if statements" $
      checkStatementExamples statement negExIf

  describe "while loop" $ do
    it "valid while statments" $
      checkStatementExamples statement exWhile
    it "invalid while statements" $
      checkStatementExamples statement negExWhile

  describe "statement sequence" $ do
    it "valid sequence" $
      checkStatementExamples statement exSeq
    it "invalid sequence" $
      checkStatementExamples statement negExSeq
