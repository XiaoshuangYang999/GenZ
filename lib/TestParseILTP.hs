-- NOTE: This test file was assisted by ChatGPT
module TestParseILTP where

import System.Directory
import System.FilePath
import Control.Monad
import FormP.ParseTPTP

-- check this directory
checkOneDir :: FilePath -> IO ()
checkOneDir dir = do
  putStrLn $ "=== Checking dir: " ++ dir ++ " ==="
  names <- listDirectory dir
  let files = [ dir </> f | f <- names, takeExtension f == ".tptp" ]
  results <- forM files $ \path -> do
    s <- readFile path
    case parseTPTPProblem s of
      Left pos -> do
        putStrLn $ "[FAIL] " ++ path ++ "  at " ++ show pos
        return False
      Right _ -> do
        putStrLn $ "[ OK ] " ++ path
        return True
  let total = length results
      oks   = length (filter id results)
      fails = total - oks
  putStrLn "----------------------------------------"
  putStrLn $ "Total : " ++ show total
  putStrLn $ "OK    : " ++ show oks
  putStrLn $ "Fail  : " ++ show fails
  putStrLn ""

-- specifically for checking "benchmarks/ILTP"
checkILTPRoot :: FilePath -> IO ()
checkILTPRoot root = do
  checkOneDir (root </> "theorem")
  checkOneDir (root </> "non_theorem")
  checkOneDir (root </> "unknown")
