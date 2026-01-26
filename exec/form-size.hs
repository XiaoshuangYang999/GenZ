module Main where

import General
import General.Lex
import FormM.Parse


main :: IO ()
main = do
  putStrLn "Give me the filepath: "
  p <- getLine
  content <- readFile p
  -- benchmarks/LWB/lwb_k/k_branch_n.txt.1.intohylo
  case parseFormM (alexScanTokens content) of
    Left e -> print e
    Right f -> do
      print $ size $ neg f