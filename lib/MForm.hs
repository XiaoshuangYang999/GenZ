{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module MForm where

import General
import PForm
import Data.List as List
-- Added temporarily for ghci test purpose
-- import K
-- import K4
-- import S4
-- import GL

a1,b1,c1,d1,e1 :: FormM
[a1,b1,c1,d1,e1] = map (AtM . return) "12345"

-- * Axioms

-- Holds in all modal logics
kaxiom :: FormM
kaxiom = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (Box b1))

-- Holds in D4,K4,S4
fouraxiom :: FormM
fouraxiom = ImpM (Box a1) (Box (Box a1))

-- Holds in T,S4
taxiom :: FormM
taxiom = ImpM (Box a1) a1

-- Holds in GL
lobaxiom :: FormM
lobaxiom = ImpM (Box (ImpM (Box a1) a1)) (Box a1)

-- Holds in D, D4, T, S4, S5
consistency :: FormM
consistency = negM . Box $ BotM

-- Holds in T, S4
density :: FormM
density = ImpM (Box (Box a1)) (Box a1)

-- The d axiom
-- Holds in D, D4, T, S4
seriality :: FormM
seriality = ImpM (Box a1) (diaM a1)

-- Holds in all modal logics
f1 :: FormM
f1 = ImpM (ConM (Box a1) (Box (ImpM a1 b1))) (Box b1)

-- Never holds.
f2 :: FormM
f2 = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (ImpM (Box b1) (Box c1)))


-- * For benchmarks

boxes :: Int -> FormM -> FormM
boxes 0 f = f
boxes n f = Box (boxes (n-1) f)

boxesTop :: Int -> FormM
boxesTop n = boxes n topM

boxesBot :: Int -> FormM
boxesBot n = boxes n BotM

-- Holds in D4, K4, S4, GL (in logics that have 4)
boxToMoreBox :: Int -> FormM
boxToMoreBox n = ImpM (boxes n a1) (boxes (n + 1) a1)

-- Holds in T,S4 (in logics that have T)
boxToFewerBox :: Int -> FormM
boxToFewerBox n = ImpM (boxes (n + 1) a1) (boxes n a1)

-- Holds only in GL
lobBoxes:: Int -> FormM
lobBoxes n = ImpM (Box (ImpM (Box a1) a1)) (boxes n a1)

-- Generate a list of n variables
listOfAt :: Int -> [FormM]
listOfAt n = map (AtM . show) $ take n [(3::Integer)..]

-- Multi-version of the K axiom
multiVerK :: Int -> FormM
multiVerK n = ImpM (Box (List.foldr ImpM (AtM "1") (listOfAt n)))
                $ foldr (ImpM . Box) (Box (AtM "1")) (listOfAt n)

-- Similar to multiVerK, but with an extra atom in the premise. False
extraAtK :: Int -> FormM
extraAtK n = ImpM (Box (List.foldr ImpM (AtM "1") (listOfAt n ++ [AtM "2"])))
                $ foldr (ImpM . Box) (Box (AtM "1")) (listOfAt n)

-- Bench formula for S4. Not provable
negBoxes :: Int -> FormM
negBoxes n = negM $ Box $ negM $ boxes n a1

propFormulasM :: [(String, Int -> FormM)]
propFormulasM =  map (fmap (pTom .)) allFormulasP

boxesFormulasM :: [(String, Int -> FormM)]
boxesFormulasM =
  [ ("boxesTop", boxesTop) -- T used to be faster than Z
  , ("boxesBot", boxesBot)
  ]

kFormulasM :: [(String, Int -> FormM)]
kFormulasM =
  [ ("multiVerK", multiVerK) -- T
  , ("boxToMoreBox", boxToMoreBox) -- F
  , ("extraAtK", extraAtK) -- F
  ]

k4FormulasM :: [(String, Int -> FormM)]
k4FormulasM =
  [ ("boxToMoreBox", boxToMoreBox) -- T
  , ("boxToFewerBox", boxToFewerBox) -- F
  ]

glFormulasM :: [(String, Int -> FormM)]
glFormulasM =
  [ ("lobBoxes", lobBoxes) -- T
  , ("boxToFewerBox", boxToFewerBox) -- F
  ]

s4FormulasM :: [(String, Int -> FormM)]
s4FormulasM =
  [ ("negBoxes", negBoxes) -- F
  ]

-- Only go until 20 or you will run out of memory.
hards4FormulasM :: [(String, Int -> FormM)]
hards4FormulasM =
  [ ("boxToFewerBox", boxToFewerBox) -- T
  , ("lobBoxes", lobBoxes) -- F
  ]
