module Main where

import System.Timeout
import Control.Exception
import System.Environment (getArgs)

import General
import General.Lex
import FormM.Parse

import K
import S4

main :: IO ()
main = do
  (input,options) <- getInputAndSettings
  let myIsProvable = if "--tree" `elem` options then isProvableT else isProvableZ
  let myLogic = if "--s4" `elem` options then sfour else k
  let limit = 20 * 1000000 -- in microseconds
  case parseFormM (alexScanTokens input) of
    Left e -> putStrLn $ "Parse error at" ++ show e
    Right f -> do
      r <- timeout limit (evaluate $ myIsProvable myLogic (negM f))
      case r of
        Nothing -> putStrLn "Timeout"
        Just b  -> print b

getInputAndSettings :: IO (String,[String])
getInputAndSettings = do
  args <- getArgs
  case args of
    ("-":options) -> do
      input <- getContents
      return (input,options)
    (filename:options) -> do
      input <- readFile filename
      return (input,options)
    _ -> do
      {-
      name <- getProgName
      mapM_ (hPutStrLn stderr)
        [ infoline
        , "usage: " ++ name ++ " <filename> {options}"
        , "       (use filename - for STDIN)\n"
        , "  -tex   generate LaTeX code\n"
        , "  -show  write to /tmp, generate PDF and show it (implies -tex)\n" ]
      -}
      return ("",[])
