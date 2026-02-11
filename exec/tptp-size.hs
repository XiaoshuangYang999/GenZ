module Main where

import General
import FormP.ParseTPTP


main :: IO ()
main = do
  putStrLn "Give me the filepath: "
  p <- getLine
  content <- readFile p
  case parseTPTPProblem content of
    Left e -> print e
    Right f -> do
      print $ size f
