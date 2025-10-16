module Main where

import General

import FormM.Lex
import FormM.Parse

-- TODO import multiple logics, qualified, add command line arg to choose it
import K

main :: IO ()
main = do
  putStrLn "Give me a formula: "
  l <- getLine
  print l
  case parseFormM (alexScanTokens l) of
    Left e -> print e
    Right f -> do
      print f
      print $ isProvableZ k f
