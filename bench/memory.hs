module Main (main) where

import System.Environment (getArgs)
import Data.List
import Weigh

import General
import Logic.IPL
import Logic.K
import Logic.K4
import Logic.GL
import Logic.CPL
import Logic.S4
import FormM
import FormP

main :: IO ()
main = do
  args <- getArgs
  mainWith $
    if "--all-formulas" `elem` args
    then do
      -- all formulas:
      makeCases [ ("CPL", classical), ("IPL", intui) ] allFormulasP [100]
      makeCases [ ("K", k), ("K4", kfour), ("GL", gl), ("S4", sfour) ] propFormulasM [10]
      makeCases [ ("K", k), ("K4", kfour), ("GL", gl), ("S4", sfour) ] boxesFormulasM [10]
      makeCases [ ("K", k) ] kFormulasM [2]
      makeCases [ ("K4", kfour) ] k4FormulasM [2]
      makeCases [ ("GL", gl) ] glFormulasM [2]
      makeCases [ ("S4", sfour) ] s4FormulasM [5]
    else do
      -- selected four formulas:
      makeCases [ ("IPL", intui) ] [("conPieL",conPieL), ("conPieR",conPieR)] [100] -- not provable
      makeCases [ ("K", k) ] [("boxesTop",boxesTop)] [1000] -- provable
      makeCases [ ("K4", kfour) ] [("lobBoxes",lobBoxes)] [10] -- not provable
      makeCases [ ("GL", gl) ] [("lobBoxes",lobBoxes)] [100] -- provable

makeCases :: (Ord f, Show f) => [(String, Logic f)] -> [(String, Int -> f)] -> [Int] -> Weigh ()
makeCases logics forms sizes = mapM_ (\ (label, logic, method, form, n) -> func label (method logic . form) n)
  [ (intercalate "|" [logicStr, formStr, methodStr, show n, show result], logic, method, formFor, n)
  | (logicStr, logic)   <- logics
  , (formStr, formFor)  <- forms
  , (methodStr, method) <- [ ("GenT", isProvableT)
                           , ("GenZ ", isProvableZ) ]
  , n <- sizes
  , let result = method logic (formFor n)
  ]
