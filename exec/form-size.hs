module Main where

import General
import General.Lex
import FormM.Parse


main :: IO ()
main = do
  putStrLn "Do you want K or S4? "
  l <- getLine
  putStrLn "Give me the formula's name: "
  name' <- getLine
  putStrLn "Give me the number: "
  num <- getLine
  content <- readFile $ "benchmarks/LWB/lwb_" ++ l ++ "/" ++ name' ++ ".txt." ++ num ++ ".intohylo"
  case parseFormM (alexScanTokens content) of
    Left e -> print e
    Right f -> do
      print $ size $ neg f