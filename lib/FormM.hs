{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module FormM where

import qualified Data.Set as Set
import GHC.Generics
import Test.QuickCheck
import Data.List as List
import General
import FormP

-- * The Modal Language

data FormM = BotM | AtM Atom | ConM FormM FormM | DisM FormM FormM | ImpM FormM FormM | Box FormM
  deriving (Eq,Ord,Generic)

instance PropLog FormM where
  neg f = ImpM f BotM
  dis = DisM
  con = ConM
  top = neg BotM
  iff f g = ConM (ImpM f g) (ImpM g f)
  -- Axiom: Γ, p ⇒ ∆, p
  isAtom (AtM _) = True
  isAtom _ = False
  isAxiom _ fs _ = [ ("ax", [])
                  | any (\f -> swap f `Set.member` fs) fs ]
  -- Rule ⊥L: from Γ, ⊥ ⇒ ∆
  leftBot _ fs _ = [ ("⊥L", []) | Left BotM `Set.member` fs ]

dia :: FormM -> FormM
dia f = neg $ Box $ neg f

instance Show FormM where
  show BotM       = "⊥"
  show (AtM a)    = a
  show (ConM f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
  show (DisM f g) = "(" ++ show f ++ " v " ++ show g ++ ")"
  show (ImpM f g) = "(" ++ show f ++ " → " ++ show g ++ ")"
  show (Box f)    = "☐" ++ show f

instance TeX FormM where
  tex BotM       = "\\bot"
  tex (AtM ('p':s)) = "p_{" ++ s ++ "}"
  tex (AtM a)    = a
  tex (ConM f g) = "(" ++ tex f ++ " \\land " ++ tex g ++ ")"
  tex (DisM f g) = "(" ++ tex f ++ " \\lor " ++ tex g ++ ")"
  tex (ImpM f g) = "(" ++ tex f ++ " \\to " ++ tex g ++ ")"
  tex (Box f)    = " \\Box " ++ tex f

instance Arbitrary FormM where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotM, AtM <$> elements (map return "pqrst")]
    genForm 1 = AtM <$> elements (map return "pqrst")
    genForm n = oneof
      [ pure BotM
      , AtM <$> elements (map return "pqrst")
      , ImpM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , DisM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , Box <$> genForm (n `div` factor)
      ]
  shrink = nub . genericShrink

a1,b1,c1,d1,e1 :: FormM
[a1,b1,c1,d1,e1] = map (AtM . return) "12345"

-- * Axioms

-- □(φ → ψ) → (□φ → □ψ) | Holds in all modal logics
kAxiom :: FormM
kAxiom = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (Box b1))

-- □φ → □□φ | Holds in D4, K4, K45, D45, S4, GL
fourAxiom :: FormM
fourAxiom = ImpM (Box a1) (Box (Box a1))

-- □φ → φ | Holds in T, S4
tAxiom :: FormM
tAxiom = ImpM (Box a1) a1

-- φ → □♢φ | Holds in B
bAxiom :: FormM
bAxiom = ImpM a1 (Box (dia a1))

-- □(□φ → φ) → □φ | Holds in GL
lobAxiom :: FormM
lobAxiom = ImpM (Box (ImpM (Box a1) a1)) (Box a1)

-- ¬□⊥ | Holds in D, D4, D45, T, S4, S5
consistency :: FormM
consistency = neg . Box $ BotM

-- □□φ → □φ | Holds in T, S4， D45
density :: FormM
density = ImpM (Box (Box a1)) (Box a1)

-- □φ → ♢φ | Holds in D, D4, D45, T, S4 | Also known as seriality
dAxiom :: FormM
dAxiom = ImpM (Box a1) (dia a1)

-- ♢φ → □♢φ | Holds in K45, D45, S5
fiveAxiom :: FormM
fiveAxiom = ImpM (dia a1) (Box (dia a1))

-- Holds in all modal logics
f1 :: FormM
f1 = ImpM (ConM (Box a1) (Box (ImpM a1 b1))) (Box b1)

-- Never holds.
f2 :: FormM
f2 = ImpM (Box (ImpM a1 b1)) (ImpM (Box a1) (ImpM (Box b1) (Box c1)))

-- * For benchmarks
-- □...□φ
boxes :: Int -> FormM -> FormM
boxes 0 f = f
boxes n f = Box (boxes (n-1) f)
-- □...□⊤
boxesTop :: Int -> FormM
boxesTop n = boxes n top
-- □...□⊥
boxesBot :: Int -> FormM
boxesBot n = boxes n BotM
-- □...□φ → □...□□φ | Holds in D4, K4, K45, S4, GL (in logics that have 4)
boxToMoreBox :: Int -> FormM
boxToMoreBox n = ImpM (boxes n a1) (boxes (n + 1) a1)
-- □...□□φ → □...□φ | Holds in T, S4, D45
boxToFewerBox :: Int -> FormM
boxToFewerBox n = ImpM (boxes (n + 1) a1) (boxes n a1)
-- □(□φ → φ) → □...□φ | Holds only in GL
lobBoxes:: Int -> FormM
lobBoxes n = ImpM (Box (ImpM (Box a1) a1)) (boxes n a1)

-- Generate a list of n variables
listOfAt :: Int -> [FormM]
listOfAt n = map (AtM . show) $ take n [(3::Integer)..]

-- Multi-version of the K Axiom
multiVerK :: Int -> FormM
multiVerK n = ImpM (Box (List.foldr ImpM (AtM "1") (listOfAt n)))
                $ foldr (ImpM . Box) (Box (AtM "1")) (listOfAt n)

-- Similar to multiVerK, but with an extra atom in the premise. False
extraAtK :: Int -> FormM
extraAtK n = ImpM (Box (List.foldr ImpM (AtM "1") (listOfAt n ++ [AtM "2"])))
                $ foldr (ImpM . Box) (Box (AtM "1")) (listOfAt n)

-- Bench formula for S4. Not provable
negBoxes :: Int -> FormM
negBoxes n = neg $ Box $ neg $ boxes n a1

-- * Embedding Propositional language into Modal language
pTom :: FormP -> FormM
pTom BotP = BotM
pTom (AtP x) = AtM x
pTom (ConP x y) = ConM (pTom x) (pTom y)
pTom (DisP x y) = DisM (pTom x) (pTom y)
pTom (ImpP x y) = ImpM (pTom x) (pTom y)

-- The Gödel–McKinsey–Tarski Translation
translation :: FormP -> FormM
translation BotP = BotM
translation (AtP x) = Box $ AtM x
translation (ConP x y) = ConM (translation x) (translation y)
translation (DisP x y) = DisM (translation x) (translation y)
translation (ImpP x y) = Box $ ImpM (translation x) (translation y)

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

-- | Positive modal logic tests (in any ml)
posModalTests :: [(String, FormM)]
posModalTests =
      [ ("k Axiom"          , kAxiom)
      , (show f1            , f1)
      , ("boxesTop 10"      , boxesTop 10)
      , ("multiVerK 5"     , multiVerK 5)
      ]

-- | Negative modal logic tests (in any ml)
negModalTests :: [(String, FormM)]
negModalTests =
      [ (show f2            , f2)
      , ("negBoxes 10"      , negBoxes 10)
      , ("boxesBot 10"      , boxesBot 10)
      , ("extraAtK 3"       , extraAtK 3)]