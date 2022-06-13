module Parse
  ( accept
  , getAccepted
  , whyNotAccepted
  , showAccepted)
where

import           Text.ParserCombinators.Parsec

-- Hier steht für einen Parser `p` der neu kombinierte Parser `p <* eof` für das
-- Ergebnis von `p` wobei `p` den ganzen Zeichen/Tokenstrom konsumieren
-- muss. `eof` steht für end of file wobei hier eher end of input gemeint ist.
-- `a <* b` nimmt 2 Parser, führ erst `a` dann `b` aus und gibt das Ergebnis von
-- `a` zurück.

accept :: Parser a -> String -> Bool
accept p s =
  case parse (p <* eof) "" s of
    Left _  -> False
    Right _ -> True

whyNotAccepted :: Parser a -> String -> IO ()
whyNotAccepted p s =
  case parse (p <* eof) "" s of
    Left msg -> print msg
    Right _  -> putStrLn (s ++ " is accepted")

showAccepted :: (Show a) => Parser a -> String -> IO ()
showAccepted p s =
  case parse (p <* eof) "" s of
    Left _       -> putStrLn (s ++ " is not accepted")
    Right result -> putStrLn ("parsed: " ++ show result)

getAccepted :: Parser a -> String -> Maybe a
getAccepted p s =
  case parse (p <* eof) "" s of
    Left _    -> Nothing
    Right val -> Just val
