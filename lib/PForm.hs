{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module PForm where

import General

top,o,p,q,r :: FormP
top = negP BotP
[o,p,q,r] = map (AtP . return) "opqr"

-- | Contradiction
contradiction :: FormP
contradiction = ConP p (negP p)

-- | Excluded middle
excludedMiddle :: FormP
excludedMiddle = DisP p (negP p)

-- | Double negation
doubleNegation :: FormP
doubleNegation = iffP (negP (negP p)) p

-- | Right Double negation
doubleNegationR :: FormP
doubleNegationR = ImpP p (negP (negP p))

-- | Pierce's Law
pierce :: FormP
pierce = ImpP (ImpP (ImpP p q) p) p

-- | Double negation of excluded middle
dnEM :: FormP
dnEM = negP $ negP excludedMiddle

-- | List of tests
t1,t2,t3,t4,t5,t6:: FormP
[t1,t2,t3,t4,t5,t6] = [ ImpP p p
                , ImpP (ImpP p (ImpP p q)) (ImpP p q)
                , ImpP (ImpP pierce q) q
                , ConP r excludedMiddle
                , negP $ negP $ ImpP p (ImpP q r)
                , negP $ negP $ DisP p $ negP q
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
conTopR k = foldr ConP topP (replicate k topP )
-- True
conTopL :: Int -> FormP
conTopL k = foldl ConP topP (replicate k topP )
-- True
disTopR :: Int -> FormP
disTopR k = foldr DisP topP (replicate k topP )
-- True
disTopL :: Int -> FormP
disTopL k = foldl DisP topP (replicate k topP )
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
