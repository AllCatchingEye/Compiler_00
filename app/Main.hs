module Main where

import Data.Semigroup ((<>))
import Interpreter (interpretCount)
import Options.Applicative
import ProgramParser (parseProgram)

data Command
  = ParseProgram
  | InterpretProgram
  deriving (Eq)

data Config = Config
  { optCommand :: Command,
    fileName :: String
  }

config :: Parser Config
config =
  Config
    <$> hsubparser
      ( command
          "parse"
          ( info
              (pure ParseProgram)
              (progDesc "Programm Parsen")
          )
          <> command
            "interpret"
            ( info
                (pure InterpretProgram)
                (progDesc "Programm interpretieren")
            )
      )
    <*> argument
      str
      ( metavar "FILENAME"
          <> help "Dateiname"
      )

main :: IO ()
main = main' =<< execParser opts
  where
    opts =
      info
        (config <**> helper)
        ( fullDesc
            <> progDesc "InterpreterMitParser für eine IMPerative Sprache"
            <> header "IMP2"
        )

main' :: Config -> IO ()
main' cfg = do
  fileContent <- readFile (fileName cfg)
  case optCommand cfg of
    ParseProgram ->
      case parseProgram fileContent of
        Left msg -> putStrLn ("parse error: " ++ show msg)
        Right p -> putStrLn (show p)
    InterpretProgram ->
      case parseProgram fileContent of
        Left msg -> putStrLn ("parse error: " ++ show msg)
        Right p ->
          case interpretCount p of
            Left msg -> putStrLn ("interpreter failed with " ++ msg)
            Right result -> putStrLn ("result: " ++ show result)
