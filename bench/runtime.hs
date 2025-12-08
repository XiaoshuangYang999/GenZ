module Main where

import Control.Monad (unless, when)
import Criterion.Main
import qualified Criterion.Types
import qualified Data.ByteString.Lazy as BL
import Data.Char (isSpace)
import Data.Csv
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Scientific
import qualified Data.Vector as V
import Numeric
import System.Directory
import System.Environment (getArgs)

import General
import Logic.CPL
import Logic.IPL
import Logic.K
import Logic.GL
import Logic.K4
import Logic.S4
import FormP
import FormM

type Case = (String, Int -> Bool, [Int])

-- | Selected formulas. Takes only a few minutes to run.
selection :: [Case]
selection =
     makeCases [ ("IPL", intui) ] [("conPie-L", conPieL), ("conPie-R", conPieR)] [10,20..100] -- not provable
  ++ makeCases [ ("K", k) ] [("boxesTop", boxesTop)] [10,20..100] -- provable
  ++ makeCases [ ("K4", kfour) ] [("lobBoxes", lobBoxes)] [1..10] -- not provable
  ++ makeCases [ ("GL", gl) ] [("lobBoxes", lobBoxes)] [1..10] -- provable

-- | Large set of formulas. Can take multple hours to run.
allFormulas :: [Case]
allFormulas =
     makeCases [("CPL", classical), ("IPL", intui) ] allFormulasP [10,20..100]
  ++ makeCases [("CPL", classical), ("IPL", intui) ] hardFormulasP [1..10]
  ++ makeCases [("K", k)] (propFormulasM ++ boxesFormulasM) [10,20..100]
  ++ makeCases [("K", k)] kFormulasM [1..10]
  ++ makeCases [("K4", kfour)] (propFormulasM ++ boxesFormulasM) [10,20..100]
  ++ makeCases [("K4", kfour)] k4FormulasM [1..8]
  ++ makeCases [("GL", gl)] propFormulasM [10,20..100]
  ++ makeCases [("S4", sfour)] propFormulasM [10,20..100]
  ++ makeCases [("S4", sfour)] hards4FormulasM [1..10]

-- | Helper function to run the maximum size of each case.
-- Ueful to adjust the ranges given above.
testAllMaxSizeItems :: IO ()
testAllMaxSizeItems = mapM_ func allItems where
  func (n1, f, range) = do
    print (n1 ++ show (maximum range))
    print $ f (maximum range)

allItems :: [Case]
allItems = nubBy sameC $ selection ++ allFormulas where
  sameC (n1, _, _) (n2, _, _) = n1 == n2

makeCases :: (Ord f, Show f) => [(String, Logic f)] -> [(String, Int -> f)] -> [Int] -> [Case]
makeCases logics forms sizes =
  [ (fS ++ "-" ++ lS ++ "-" ++ pS , prover logic . formula, sizes)
  | (fS, formula) <- forms
  , (pS, prover) <- [("GenZ", isProvableZ), ("GenT", isProvableT)]
  , (lS, logic) <- logics ]

benchMain :: IO ()
benchMain =
  defaultMainWith myConfig (map mybench allItems) where
  mybench (name1,f,range) = bgroup name1 $ map (run f) range
  run f n = bench (show n) $ whnf f n
  myConfig = defaultConfig
    { Criterion.Types.csvFile = Just theCSVname
    , Criterion.Types.timeLimit = 10 }

main :: IO ()
main = do
  args <- getArgs
  unless ("--list" `elem` args) prepareMain
  benchMain
  unless ("--list" `elem` args) convertMain

-- * CSV to pgfplots

-- | The filename to which the benchmark results will be written in CSV.
theCSVname :: String
theCSVname = "bench/results.csv"

prepareMain :: IO ()
prepareMain = do
  oldResults <- doesFileExist theCSVname
  when oldResults $ do
    putStrLn "Note: moving away old results."
    renameFile theCSVname (theCSVname ++ ".OLD")
    oldDATfile <- doesFileExist (theCSVname ++ ".dat")
    when oldDATfile $ removeFile (theCSVname ++ ".dat")

-- | Convert the .csv file to a .dat file to be used with pgfplots.
convertMain :: IO ()
convertMain = do
  putStrLn "Reading results.csv and converting to .dat for pgfplots."
  c <- BL.readFile theCSVname
  case decode NoHeader c of
    Left err -> error $ "could not parse the csv file:" ++ show err
    Right csv -> do
      let results = map (parseLine . take 2) $ tail $ V.toList (csv :: V.Vector [String])
      let columns = nub.sort $ map (fst.fst) results
      let widthNeeded = maximum $ map length columns
      let longify = longifyTo (widthNeeded + 2)
      let firstLine = longifyTo 5 "n" ++ dropWhileEnd isSpace (concatMap longify columns)
      let resAt n col = longify $ fromMaybe "nan" $ Data.List.lookup (col,n) results
      let resultrow n = concatMap (resAt n) columns
      let firstcol = nub.sort $ map (snd.fst) results
      let resultrows = map (\n -> longifyTo 5 (show n) ++ dropWhileEnd isSpace (resultrow n)) firstcol
      writeFile (theCSVname ++ ".dat") (intercalate "\n" (firstLine:resultrows) ++ "\n")
  where
    parseLine [namestr,numberstr] = case splitOn "/" namestr of
      [name1,nstr] -> ((name1,n),valuestr) where
        n = read nstr :: Integer
        value = toRealFloat (read numberstr :: Scientific) :: Double
        valuestr = Numeric.showFFloat (Just 7) value ""
      _ -> error $ "could not parse this case: " ++ namestr
    parseLine l = error $ "could not parse this line:\n  " ++ show l
    longifyTo n s = s ++ replicate (n - length s) ' '
