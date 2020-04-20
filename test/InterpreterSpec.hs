module InterpreterSpec
  ( spec )
where

import           Test.Hspec
import           Test.QuickCheck (property)

import           Interpreter (interpret)
import           Types (AExpr (..), BExpr (..), Program (..), ProgramState, Statement (..),
                     VarName (..), declareVar, emptyProgramState, updateVar)
import           Util

exArith :: ExprTestPairs Integer
exArith = [ ("1", Just 1)
          , ("-1", Just (-1))
          , ("1 + 1", Just 2)
          , ("1 - 1", Just 0)
          , ("5 / 2", Just 2)
          , ("5 / -2", Just (-3))
          , ("5 % 2", Just 1)
          , ("5 % -2", Just (-1))
          , ("1 + 2 * 5", Just 11)
          , ("2 * 5 + 1", Just 11)
          , ("2 * (5 + 1)", Just 12)
          , ("(1 + 2) * 5", Just 15)
          , ("5*3/2", Just 7)
          ]

exBool :: ExprTestPairs Bool
exBool = [ ("tt", Just True)
         , ("ff", Just False)
         , ("tt||ff", Just True)
         , ("tt && ff || tt", Just True)
         , ("tt && ff || tt", Just True)
         , ("ff || tt && ff", Just False)
         , ("(ff || tt) && ff", Just False)
         , ("not tt", Just False)
         , ("not ff", Just True)
         , ("not (ff || ff) && tt", Just True)
         , ("(ff || tt) && not ff", Just True)
         ]

exCmp :: ExprTestPairs Bool
exCmp = [ ( "1 < 2", Just True)
        , ( "1 > 2", Just False)
        , ( "1 = 2", Just False)
        , ( "2 = 2", Just True)
        , ( "1 + 2 < 3 + 4", Just True)
        , ( "1 < 1 + 2 + 3", Just True)
        , ( "1 + 2 + 3 < 1", Just False)
        ]

exCmpVar :: ExprTestPairs Bool
exCmpVar = [ ("a < b", Just True)
           , ("a < c", Just False)
           , ("-1 <= c", Just True)
           , ("2+4*2 = a+b-  c  -c  -c", Just True)
           ]

initProgramState' :: Maybe ProgramState
initProgramState' = do
  withA <- declareVar emptyProgramState (VarName "a")
  withB <- declareVar withA (VarName "b")
  withC <- declareVar withB (VarName "c")
  setA  <- updateVar  withC (VarName "a") 2
  setB  <- updateVar  setA  (VarName "b") 5
  updateVar setB (VarName "c") (-1)

initProgramState :: ProgramState
initProgramState =
  case initProgramState' of
    Just p -> p
    Nothing -> error "cannot insert values into empty program state"

spec :: Spec
spec = do
  describe "arithmethic expressions" $ do
    it "valid expressions" $
      checkAExprExamples emptyProgramState exArith

  describe "Boolean expressions" $ do
    it "valid expressions" $
      checkBExprExamples emptyProgramState exBool

  describe "Comparison expressions" $ do
    it "valid expressions" $
      checkBExprExamples emptyProgramState exCmp

    it "valid expressions with variables" $
      checkBExprExamples initProgramState exCmpVar

  describe "Full program interpretation" $ do
    it "square and multiply" $
      property $ \x n ->
      (interpret $ exSquareAndMultiply x (abs n)) `shouldBe` (Right $ x^(abs n))

    it "exponentiate" $
      property $ \x n ->
      (interpret $ exExponentiate x (abs n)) `shouldBe` (Right $ x^(abs n))

    it "Fibonacci" $
      property $ \n ->
      (interpret $ exFibonacci (abs n)) `shouldBe` (Right $ fib 0 1 (abs n))

exSquareAndMultiply :: Integer -> Integer -> Program
exSquareAndMultiply x n = Program {
    declaration = [
      VarName {getVarName = "x"}
    ,VarName {getVarName = "y"}
    ,VarName {getVarName = "n"}
  ]
  , statements = StmtSeq {
      first = StmtAssign {lhs = VarName {getVarName = "x"}, rhs = AExprInt x}
    , next = StmtSeq {
        first = StmtAssign {lhs = VarName {getVarName = "n"}, rhs = AExprInt n}
      , next = StmtSeq {
          first = StmtIf {
            ifCondition = BExprEq (AExprVar (VarName {getVarName = "n"})) (AExprInt 0)
          , thenCase = StmtReturn {resultValue = AExprInt 1}
          , elsecase = StmtSkip
        }
        , next = StmtSeq {
            first = StmtAssign {lhs = VarName {getVarName = "y"}, rhs = AExprInt 1}
          , next = StmtSeq {
              first = StmtWhile {
                loopCondition = BExprGT (AExprVar (VarName {getVarName = "n"})) (AExprInt 1)
              , loopBody = StmtIf {
                  ifCondition = BExprEq (AExprMod (AExprVar (VarName {getVarName = "n"})) (AExprInt 2)) (AExprInt 0)
                , thenCase = StmtSeq {
                    first = StmtAssign {
                      lhs = VarName {getVarName = "x"}
                    , rhs = AExprMult (AExprVar (VarName {getVarName = "x"})) (AExprVar (VarName {getVarName = "x"}))
                  }
                  , next = StmtAssign {
                      lhs = VarName {getVarName = "n"}
                    , rhs = AExprDiv (AExprVar (VarName {getVarName = "n"})) (AExprInt 2)
                  }
                }
                , elsecase = StmtSeq {
                    first = StmtAssign {
                      lhs = VarName {getVarName = "y"}
                    , rhs = AExprMult (AExprVar (VarName {getVarName = "y"})) (AExprVar (VarName {getVarName = "x"}))
                  }
                  , next = StmtAssign {
                      lhs = VarName {getVarName = "n"}
                    , rhs = AExprMinus (AExprVar (VarName {getVarName = "n"})) (AExprInt 1)
                  }
                }
              }
            }
            , next = StmtReturn {
                resultValue = AExprMult (AExprVar (VarName {getVarName = "x"})) (AExprVar (VarName {getVarName = "y"}))
            }
          }
        }
      }
    }
  }
}

exExponentiate :: Integer -> Integer -> Program
exExponentiate x n = Program {
    declaration = [
      VarName {getVarName = "x"}
    ,VarName {getVarName = "y"}
    ,VarName {getVarName = "n"}
  ]
  , statements = StmtSeq {
      first = StmtAssign {lhs = VarName {getVarName = "x"}, rhs = AExprInt x}
    , next = StmtSeq {
        first = StmtAssign {lhs = VarName {getVarName = "n"}, rhs = AExprInt n}
      , next = StmtSeq {
          first = StmtIf {
            ifCondition = BExprLT (AExprVar (VarName {getVarName = "n"})) (AExprInt 0)
          , thenCase = StmtReturn {resultValue = AExprInt 0}
          , elsecase = StmtIf {
              ifCondition = BExprEq (AExprVar (VarName {getVarName = "n"})) (AExprInt 0)
            , thenCase = StmtReturn {resultValue = AExprInt 1}
            , elsecase = StmtSkip
          }
        }
        , next = StmtSeq {
            first = StmtAssign {
              lhs = VarName {getVarName = "y"}
            , rhs = AExprVar (VarName {getVarName = "x"})
          }
          , next = StmtSeq {
              first = StmtWhile {
                loopCondition = BExprGT (AExprVar (VarName {getVarName = "n"})) (AExprInt 1)
              , loopBody = StmtSeq {
                  first = StmtAssign {
                    lhs = VarName {getVarName = "y"}
                  , rhs = AExprMult (AExprVar (VarName {getVarName = "y"})) (AExprVar (VarName {getVarName = "x"}))
                }
                , next = StmtAssign {
                    lhs = VarName {getVarName = "n"}
                  , rhs = AExprMinus (AExprVar (VarName {getVarName = "n"})) (AExprInt 1)
                }
              }
            }
            , next = StmtReturn {resultValue = AExprVar (VarName {getVarName = "y"})}
          }
        }
      }
    }
  }
}

exFibonacci :: Integer -> Program
exFibonacci n = Program {
    declaration = [
      VarName {getVarName = "n"}
    ,VarName {getVarName = "a"}
    ,VarName {getVarName = "b"}
    ,VarName {getVarName = "t"}
  ]
  , statements = StmtSeq {
      first = StmtAssign {lhs = VarName {getVarName = "n"}, rhs = AExprInt n}
    , next = StmtSeq {
        first = StmtIf {
          ifCondition = BExprEq (AExprVar (VarName {getVarName = "n"})) (AExprInt 0)
        , thenCase = StmtReturn {resultValue = AExprInt 0}
        , elsecase = StmtIf {
            ifCondition = BExprEq (AExprVar (VarName {getVarName = "n"})) (AExprInt 1)
          , thenCase = StmtReturn {resultValue = AExprInt 1}
          , elsecase = StmtSkip
        }
      }
      , next = StmtSeq {
          first = StmtAssign {lhs = VarName {getVarName = "a"}, rhs = AExprInt 1}
        , next = StmtSeq {
            first = StmtAssign {lhs = VarName {getVarName = "b"}, rhs = AExprInt 1}
          , next = StmtSeq {
              first = StmtWhile {
                loopCondition = BExprGT (AExprVar (VarName {getVarName = "n"})) (AExprInt 2)
              , loopBody = StmtSeq {
                  first = StmtAssign {
                    lhs = VarName {getVarName = "t"}
                  , rhs = AExprVar (VarName {getVarName = "a"})
                }
                , next = StmtSeq {
                    first = StmtAssign {
                      lhs = VarName {getVarName = "a"}
                    , rhs = AExprVar (VarName {getVarName = "b"})
                  }
                  , next = StmtSeq {
                      first = StmtAssign {
                        lhs = VarName {getVarName = "b"}
                      , rhs = AExprPlus (AExprVar (VarName {getVarName = "b"})) (AExprVar (VarName {getVarName = "t"}))
                    }
                    , next = StmtAssign {
                        lhs = VarName {getVarName = "n"}
                      , rhs = AExprMinus (AExprVar (VarName {getVarName = "n"})) (AExprInt 1)
                    }
                  }
                }
              }
            }
            , next = StmtReturn {resultValue = AExprVar (VarName {getVarName = "b"})}
          }
        }
      }
    }
  }
}
