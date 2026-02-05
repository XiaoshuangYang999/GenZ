module Main where

import General
import General.Lex
import FormP.Parse
import FormP.ParseTPTP


main :: IO ()
main = do
  putStrLn "Give me the filepath: "
  p <- getLine
  content <- readFile p
  -- benchmarks/ILTP/non_theorem/LCL181+1_n.tptp
  case parseTPTPProblem content of
    Left e -> print e
    Right f -> do
      print $ size f
