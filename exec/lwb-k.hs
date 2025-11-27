module Main where

import System.Timeout
import Control.Exception

import General
import General.Lex
import FormM.Parse

-- TODO import multiple logics, qualified, add command line arg to choose it
import K

main :: IO ()
main = do
  -- Set a time limit.
  -- Test cases will be discarded if they take more than 5 seconds.
  let limit = 5 * 1000000 -- in microseconds
  -- content <- readFile "benchmarks/LWB/lwb_k/k_dum_p.txt.1.intohylo"--benchmark/lwb_k/
  -- putStrLn "k_dum_p 1"
  -- case parseFormM (alexScanTokens content) of
  --   Left e -> print e
  --   Right f -> do
  --     r <- timeout limit (evaluate $ isProvableZ k (negM f))
  --     case r of
  --       Nothing -> putStrLn "Timeout"
  --       Just b  -> print b
  -- content2 <- readFile "benchmarks/LWB/lwb_k/k_dum_n.txt.1.intohylo"--benchmark/lwb_k/
  -- putStrLn "k_dum_n 1"
  -- case parseFormM (alexScanTokens content2) of
  --   Left e -> print e
  --   Right f -> do
  --     r <- timeout limit (evaluate $ isProvableZ k (negM f))
  --     case r of
  --       Nothing -> putStrLn "Timeout"
  --       Just b  -> print b
  -- putStrLn "Give me a formula: "
  -- l <- getLine
  -- print l
  -- case parseFormM (alexScanTokens l) of
  --   Left e -> print e
  --   Right f -> do
  --     print f
  --     print $ isProvableZ k f
  putStrLn "Give me the formula's name: "
  name1 <- getLine
  putStrLn "Give me the size of the formula: "
  size <- getLine
  content <- readFile $ "benchmarks/LWB/lwb_k/" ++ name1 ++ ".txt." ++ size ++ ".intohylo"
  case parseFormM (alexScanTokens content) of
    Left e -> print e
    Right f -> do
      putStr "GenZ: "
      r <- timeout limit (evaluate $ isProvableZ k (neg f))
      case r of
        Nothing -> putStrLn "Timeout"
        Just b  -> print b
      putStr "GenT: "
      r2 <- timeout limit (evaluate $ isProvableT k (neg f))
      case r2 of
        Nothing -> putStrLn "Timeout"
        Just b  -> print b
