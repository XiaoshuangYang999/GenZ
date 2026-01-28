module TestParseTPTP where

import System.Directory
  ( listDirectory
  )
import System.FilePath
  ( (</>)
  , takeExtension
  )
import Control.Monad
  ( forM )
import Data.List (isSuffixOf)

import FormP.Parse
  ( parseTPTPProblem
  , ParseResult
  )
import FormP (FormP)

-- 只检查一个目录（不递归，因为你这层下面已经全是文件）
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

-- 专门针对 benchmarks/ILTP 这三个文件夹
checkILTPRoot :: FilePath -> IO ()
checkILTPRoot root = do
  checkOneDir (root </> "theorem")
  checkOneDir (root </> "non_theorem")
  checkOneDir (root </> "unknown")
