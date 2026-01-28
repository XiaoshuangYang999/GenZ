module TestTPTP where

import FormP.Parse

test1 :: FilePath -> IO ()
test1 path = do
  s <- readFile path
  case FormP.Parse.parseTPTPProblem s of
    Left pos   -> putStrLn $ "Parse error at " ++ show pos
    Right form -> print form