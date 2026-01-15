{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module FormP where

import Data.List as List
import qualified Data.Set as Set
import GHC.Generics
import Test.QuickCheck
import General

-- | Propositional Formulas
data FormP = BotP | AtP Atom | ConP FormP FormP | DisP FormP FormP | ImpP FormP FormP
  deriving (Eq,Ord,Generic)

instance PropLog FormP where
  neg f = ImpP f BotP
  dis :: FormP -> FormP -> FormP
  dis = DisP
  con = ConP
  top = neg BotP
  iff f g = ConP (ImpP f g) (ImpP g f)
  -- Axiom: Γ, p ⇒ ∆, p
  isAtom (AtP _) = True
  isAtom _ = False
  isAxiom _ fs _ = [ ("ax", [])
                  | any (\f -> swap f `Set.member` fs) fs ]
  -- Rule ⊥L: from Γ, ⊥ ⇒ ∆
  leftBot _ fs _ = [ ("⊥L", []) | Left BotP `Set.member` fs ]
  size BotP         = 1
  size (AtP _)      = 1
  size (ConP f g)   = 1 + size f + size g
  size (DisP f g)   = 1 + size f + size g
  size (ImpP f g)   = 1 + size f + size g
  subFormulas BotP         = [BotP]
  subFormulas (AtP a)      = [AtP a]
  subFormulas (ConP f g)   = ConP f g : (subFormulas f ++ subFormulas g)
  subFormulas (DisP f g)   = DisP f g : (subFormulas f ++ subFormulas g)
  subFormulas (ImpP f g)   = ImpP f g : (subFormulas f ++ subFormulas g)

instance Show FormP where
  show BotP       = "⊥"
  show (AtP a)    = a
  show (ConP f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
  show (DisP f g) = "(" ++ show f ++ " v " ++ show g ++ ")"
  show (ImpP f g) = "(" ++ show f ++ " → " ++ show g ++ ")"

instance TeX FormP where
  tex BotP       = "\\bot"
  tex (AtP ('p':s)) = "p_{" ++ s ++ "}"
  tex (AtP a)    = a
  tex (ConP f g) = "(" ++ tex f ++ " \\land " ++ tex g ++ ")"
  tex (DisP f g) = "(" ++ tex f ++ " \\lor " ++ tex g ++ ")"
  tex (ImpP f g) = "(" ++ tex f ++ " \\to " ++ tex g ++ ")"

instance Arbitrary FormP where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotP, AtP <$> elements (map return "pqrst") ]
    genForm 1 = AtP <$> elements (map return "pqrst")
    genForm n = oneof
      [ pure BotP
      , AtP <$> elements (map return "pqrst")
      , ImpP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , DisP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      ]
  shrink = nub . genericShrink

o,p,q,r :: FormP
[o,p,q,r] = map (AtP . return) "opqr"

-- | Contradiction
contradiction :: FormP
contradiction = ConP p (neg p)

-- | Excluded middle
excludedMiddle :: FormP
excludedMiddle = DisP p (neg p)

-- | Double negation
doubleNegation :: FormP
doubleNegation = iff (neg (neg p)) p

-- | Right Double negation
doubleNegationR :: FormP
doubleNegationR = ImpP p (neg (neg p))

-- | Pierce's Law
pierce :: FormP
pierce = ImpP (ImpP (ImpP p q) p) p

-- | Double negation of excluded middle
dnEM :: FormP
dnEM = neg $ neg excludedMiddle

-- | List of tests
t1,t2,t3,t4,t5,t6:: FormP
[t1,t2,t3,t4,t5,t6] = [ ImpP p p
                , ImpP (ImpP p (ImpP p q)) (ImpP p q)
                , ImpP (ImpP pierce q) q
                , ConP r excludedMiddle
                , neg $ neg $ ImpP p (ImpP q r)
                , neg $ neg $ DisP p $ neg q
                ]

-- True in IPL
phi :: FormP
phi = ImpP (ConP p (ImpP p q)) (ImpP (ImpP p q) q)

-- * For benchmarks
-- False
conBotR :: Int -> FormP
conBotR k = foldr ConP BotP (replicate k BotP )
-- False
conBotL :: Int -> FormP
conBotL k = foldl ConP BotP (replicate k BotP )
-- False
disBotR :: Int -> FormP
disBotR k = foldr DisP BotP (replicate k BotP )
-- False
disBotL :: Int -> FormP
disBotL k = foldl DisP BotP (replicate k BotP )
-- True
conTopR :: Int -> FormP
conTopR k = foldr ConP top (replicate k top )
-- True
conTopL :: Int -> FormP
conTopL k = foldl ConP top (replicate k top )
-- True
disTopR :: Int -> FormP
disTopR k = foldr DisP top (replicate k top )
-- True
disTopL :: Int -> FormP
disTopL k = foldl DisP top (replicate k top )
-- True in CPL, false in IPL
conPieR :: Int -> FormP
conPieR k = foldr ConP pierce (replicate (2*k) pierce )
-- True in CPL, false in IPL
conPieL :: Int -> FormP
conPieL k = foldl ConP pierce (replicate (2*k) pierce )
-- True in CPL, false in IPL
disPieR :: Int -> FormP
disPieR k = foldr DisP pierce (replicate (2*k) pierce )
-- True in CPL, false in IPL
disPieL :: Int -> FormP
disPieL k = foldl DisP pierce (replicate (2*k) pierce )
-- True in CPL, IPL
disPhiPieR :: Int -> FormP
disPhiPieR k = foldr DisP phi (replicate (2*k) pierce )
-- True in CPL, IPL
disPhiPieL :: Int -> FormP
disPhiPieL k = foldl DisP phi (replicate (2*k) pierce )
-- True in CPL, false in IPL
phiImpPie :: Int -> FormP
phiImpPie 0 = pierce
phiImpPie n = ImpP phi $ phiImpPie (n-1)

allFormulasP :: [(String, Int -> FormP)]
allFormulasP =
  [ ("disPhiPie-R", disPhiPieR)
  , ("disPhiPie-L", disPhiPieL)
  , ("disPie-R", disPieR)
  , ("disPie-L", disPieL)
  , ("conPie-R", conPieR)
  , ("conPie-L", conPieL)
  , ("conBot-R", conBotR)
  , ("conBot-L", conBotL)
  , ("disBot-R", disBotR)
  , ("disBot-L", disBotL)
  , ("conTop-R", conTopR)
  , ("conTop-L", conTopL)
  , ("disTop-R", disTopR)
  , ("disTop-L", disTopL)
  ]

-- | Only go until 20 or you will run out of memory.
hardFormulasP :: [(String, Int -> FormP)]
hardFormulasP =
   [ ("phiImpPie", phiImpPie) ]

-- * Test formulas
-- | Positive classical propositional logic tests
posCPropTests :: [(String, FormP)]
posCPropTests =
      [ ( "Top"                                              , top )
      , ( "Double negation: " ++ show doubleNegation         , doubleNegation )
      , ( "Double negation right: " ++ show doubleNegationR  , doubleNegationR )
      , ( "Excluded middle: " ++ show excludedMiddle         , excludedMiddle )
      , ( "Pierce's law: " ++ show pierce                    , pierce )
      , ( "Double negation of excluded middle " ++ show dnEM , dnEM )
      , ( show phi                                           , phi )
      , ( show t1                                            , t1 )
      , ( show t2                                            , t2 )
      , ( show t3                                            , t3 )
      , ( "conTopR 10"                                       , conTopR 10 )
      , ( "conTopL 10"                                       , conTopL 10 )
      , ( "disTopR 10"                                       , disTopR 10 )
      , ( "disTopL 10"                                       , disTopL 10 )
      , ( "conPieR 10"                                       , conPieR 10 )
      , ( "conPieL 10"                                       , conPieL 10 )
      , ( "disPieR 10"                                       , disPieR 10 )
      , ( "disPieL 10"                                       , disPieL 10 )
      , ( "disPhiPieR 10"                                    , disPhiPieR 10 )
      , ( "disPhiPieL 10"                                    , disPhiPieL 10 )
      , ( "phiImpPie 10"                                     , phiImpPie 10 )
      ]

-- Negative classical propositional logic tests
negCPropTests :: [(String, FormP)]
negCPropTests =
      [ ( "Bot"                , BotP)
      , ( show contradiction   , contradiction)
      , ( show t4              , t4)
      , ( show t5              , t5)
      , ( show t6              , t6)
      , ( "conBotR 10"         , conBotR 10)
      , ( "conBotL 10"         , conBotL 10)
      , ( "disBotR 10"         , disBotR 10)
      , ( "disBotL 10"         , disBotL 10)
      ]
