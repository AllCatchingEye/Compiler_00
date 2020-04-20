module ProgramSpec
  ( spec )
where

import           Test.Hspec
import           Test.QuickCheck (property)

import           Util


ex1 :: Integer -> Integer -> ProgramTest
ex1 x n =
  ("decl x, y, n;\
  \ x := " ++ show x ++ ";\
  \ n:= " ++ show n ++ ";\
  \if(n < 0) {\
  \return 0;\
  \} else {\
  \  if(n = 0) { return 1; } else { skip; };\
  \};\
  \y := x;\
  \while (n > 1) {\
  \  y := y * x;\
  \  n := n - 1;\
  \};\
  \return y;", Just (if n < 0 then 0 else x^n))

ex2 :: Integer -> ProgramTest
ex2 n = ("decl n, a, b, t;\
\n := " ++ show n ++ ";\
\if (n = 0) {\
  \return 0;\
\} else {\
  \if (n = 1) {\
    \return 1;\
  \}\
  \else { skip; };\
\};\
\a := 1;\
\b := 1;\
\while (n > 2) {\
  \t := a;\
  \a := b;\
  \b := b + t;\
  \n := n - 1;\
\};\
\return b;", Just $ fib 0 1 n)

ex3 :: Integer -> Integer -> ProgramTest
ex3 x n = ("decl x, y, n;\
\x := " ++ show x ++ ";\
\n := " ++ show n ++ ";\
\if(n = 0) {\
  \return 1;\
\}\
\else {\
 \skip;\
\};\
\y := 1;\
\while (n > 1) {\
  \if (n % 2 = 0) {\
    \x := x * x;\
    \n := n / 2;\
  \} else {\
    \y := y * x;\
    \n := n - 1;\
  \};\
\};\
\return x * y;", Just (x^n))

spec :: Spec
spec = do
  describe "parse and evaluate programs" $ do
    it "simple exponentiation" $
      property $ \x n -> checkProgram (ex1 x n)
    it "Fibonacci" $
      property $ \n -> checkProgram (ex2 (abs n))
    it "square and multiply" $
      property $ \x n -> checkProgram (ex3 x (abs n))
