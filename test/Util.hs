module Util

where

import           Text.ParserCombinators.Parsec (Parser, eof, parse)

import           Test.Hspec (shouldBe)
import           Test.HUnit.Base (assertFailure)

import           AExpParser (aExpr)
import           BExpParser (bExpr)
import           Interpreter (interpret)
import           Interpreter (interpretAexpr, interpretBexpr)
import           ProgramParser (parseProgram)
import           Types (AExpr, BExpr, ProgramState)

type TestPairs a = [(String, Maybe a)]

parseSuccess :: Parser a -> String -> Maybe a
parseSuccess p s = case parse (p <* eof) "" s of
                     Left _  -> Nothing
                     Right x -> Just x

checkStatementExamples :: (Show a, Eq a) => Parser a -> TestPairs a -> IO ()
checkStatementExamples p ex =
  (map ((parseSuccess p) . fst) ex) `shouldBe` (map snd ex)

type ExprTestPairs a = [(String, Maybe a)]

checkAExprExamples :: ProgramState -> ExprTestPairs Integer -> IO()
checkAExprExamples p ex =
  (map (\(s, _) -> (interpretAexpr p) <$> parseAexpr s) ex)
  `shouldBe` (map snd ex)

parseAexpr :: String -> Maybe AExpr
parseAexpr = parseExpr aExpr

parseBexpr :: String -> Maybe BExpr
parseBexpr = parseExpr bExpr

parseExpr :: Parser a -> String -> Maybe a
parseExpr p s = case parse p "" s of
                  Left _  -> Nothing
                  Right v -> Just v

checkBExprExamples :: ProgramState -> ExprTestPairs Bool -> IO()
checkBExprExamples p ex =
  (map (\(s, _) -> (interpretBexpr p) <$> parseBexpr s) ex)
  `shouldBe` (map snd ex)

type ProgramTest = (String, Maybe Integer)

checkProgram :: ProgramTest -> IO ()
checkProgram (s, i) = do
  case parseProgram s of
    Left msg  -> assertFailure ("could not be parsed: " ++ show msg)
    Right p -> case interpret p of
                 Left _ -> assertFailure "could not be interpreted"
                 Right result -> Just result `shouldBe` i

fib :: Integer -> Integer -> Integer -> Integer
fib a b n = if n == 0 then a else fib b (a + b) (n - 1)
