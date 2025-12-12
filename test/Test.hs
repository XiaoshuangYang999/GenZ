module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Bifunctor

import General
import FormP
import FormM
import Logic.Propositional.CPL
import Logic.Propositional.IPL
import Logic.Modal.K
import Logic.Modal.K4
import Logic.Modal.GL
import Logic.Modal.S4
import Logic.Modal.T
import Logic.Modal.D
import Logic.Modal.D4
import Logic.Modal.K45

-- | Set a time limit.
-- Test cases will be discarded if they take more than 5 seconds.
limit :: Int
limit = 5 * 1000000 -- in microseconds

-- | Ensure laziness
implies :: Bool -> Bool -> Bool
implies x = if x then id else const True

testsFor :: (Show f, Ord f) => Logic f -> [(String,f)] -> [(String,f)] -> SpecWith ()
testsFor l posExamples negExamples = do
  describe (name l) $ do
    describe "isProvableZ" $ do
      mapM_ (\(s, f) -> it s $ isProvableZ l f) posExamples
    describe "not.isProvableZ" $ do
      mapM_ (\(s, f) -> it s $ not $ isProvableZ l f) negExamples
    describe "isProvableT" $ do
      mapM_ (\(s, f) -> it s $ isProvableT l f) posExamples
    describe "not.isProvableT" $ do
      mapM_ (\(s, f) -> it s $ not $ isProvableT l f) negExamples

conCheck :: (Arbitrary f, Show f, Ord f, PropLog f) => [Logic f] -> SpecWith ()
conCheck = mapM_ $ \l -> do
  prop ("GenZ for " ++ name l) $
    \ f g -> discardAfter limit $ (isProvableZ l f && isProvableZ l g) `implies` isProvableZ l (con f g)
  prop ("GenT for " ++ name l) $
    \ f g -> discardAfter limit $ (isProvableT l f && isProvableT l g) `implies` isProvableT l (con f g)

containTest :: (Arbitrary f, Show f, Ord f, PropLog f) => Logic f -> Logic f -> SpecWith ()
containTest l1 l2 =
  describe ("What is provable in " ++ name l1 ++  " is also provable in " ++ name l2) $ do
    prop "GenZ" $
      \ f -> discardAfter limit $ isProvableZ l1 f `implies` isProvableZ l2 f
    prop "GenT" $
      \ f -> discardAfter limit $ isProvableT l1 f `implies` isProvableT l2 f

agreeTestTranslated :: (Arbitrary f, Show f, Ord f, PropLog f, Arbitrary f', Show f', Ord f', PropLog f')  => Logic f -> Logic f' -> (f -> f') -> SpecWith ()
agreeTestTranslated l1 l2 tr = do
  prop "GenZ" $
    \ f -> discardAfter limit $ isProvableZ l1 f === isProvableZ l2 (tr f)
  prop "GenT" $
    \ f -> discardAfter limit $ isProvableT l1 f === isProvableT l2 (tr f)

proverEqTest :: (Arbitrary f, Show f, Ord f, PropLog f) => Logic f -> SpecWith ()
proverEqTest l = do
  prop (name l) $
        \ f -> discardAfter limit $ isProvableZ l f === isProvableT l f

atMostBinTest :: (Arbitrary f, Show f, Ord f, PropLog f) => Logic f -> SpecWith ()
atMostBinTest l = do
        let hasLeqTwoChildren (Node _ Nothing _) = True
            hasLeqTwoChildren (Node _ (Just (_, ts)) _ ) = length ts <= 2 && all hasLeqTwoChildren ts
        prop ("GenZ for " ++ name l) $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ l f
        prop ("GenT for " ++ name l) $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT l f

main :: IO ()
main = hspec $ parallel $ do
  describe "Unit tests" $ do
    testsFor classical posCPropTests negCPropTests
    testsFor intui
      [ ( "Top"                                              , top )
      , ( "Double negation right: " ++ show doubleNegationR  , doubleNegationR )
      , ( "Double negation of excluded middle " ++ show dnEM , dnEM )
      , ( show phi                                           , phi )
      , ( show t1                                            , t1 )
      , ( show t2                                            , t2 )
      , ( show t3                                            , t3 )
      , ( "conTopR 10"                                       , conTopR 10 )
      , ( "conTopL 10"                                       , conTopL 10 )
      , ( "disTopR 10"                                       , disTopR 10 )
      , ( "disTopL 10"                                       , disTopL 10 )
      , ( "disPhiPieR 10"                                    , disPhiPieR 10 )
      , ( "disPhiPieL 10"                                    , disPhiPieL 10 )
      ]
      [ ( "Bot"                                             , BotP)
      , ( show contradiction                                , contradiction)
      , ("Double negation: " ++ show doubleNegation         , doubleNegation)
      , ("Excluded middle: " ++ show excludedMiddle         , excludedMiddle)
      , ("Pierce's law: " ++ show pierce                    , pierce)
      , ( show t4                                           , t4)
      , ( show t5                                           , t5)
      , ( show t6                                           , t6)
      , ( "conBotR 10"                                      , conBotR 10)
      , ( "conBotL 10"                                      , conBotL 10)
      , ( "disBotR 10"                                      , disBotR 10)
      , ( "disBotL 10"                                      , disBotL 10)
      , ( "conPieR 10"                                      , conPieR 10)
      , ( "conPieL 10"                                      , conPieL 10)
      , ( "disPieR 10"                                      , disPieR 10)
      , ( "disPieL 10"                                      , disPieL 10)
      , ( "phiImpPie 10"                                    , phiImpPie 10)
      ]
    testsFor k
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests)
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("4 Axiom"           , fourAxiom)
                    , ("Lob Axiom"         , lobAxiom)
                    , ("t Axiom"           , tAxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("d Axiom"           , dAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("lobBoxes 5"      , lobBoxes 5)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("boxToFewerBox 5"  , boxToFewerBox 5)
                    ])
    testsFor kfour
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("4 Axiom"          , fourAxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob Axiom"         , lobAxiom)
                    , ("t Axiom"           , tAxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("d Axiom"           , dAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor t
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("t Axiom"           , tAxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("d Axiom"           , dAxiom)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob Axiom"         , lobAxiom)
                    , ("4 Axiom"           , fourAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
    testsFor d
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("Consistency"       , consistency)
                    , ("d Axiom"           , dAxiom)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob Axiom"         , lobAxiom)
                    , ("t Axiom"           , tAxiom)
                    , ("4 Axiom"           , fourAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("Density"           , density)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor dfour
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("4 Axiom"          , fourAxiom)
                    , ("Consistency"      , consistency)
                    , ("d Axiom"          , dAxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob Axiom"         , lobAxiom)
                    , ("t Axiom"           , tAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("Density"           , density)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor sfour
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("4 Axiom"          , fourAxiom)
                    , ("t Axiom"           , tAxiom)
                    , ("Density"           , density)
                    , ("Consistency"       , consistency)
                    , ("dAxiom"            , dAxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob Axiom"         , lobAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    ])
    testsFor gl
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("4 Axiom"          , fourAxiom)
                    , ("Lob Axiom"         , lobAxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("t Axiom"           , tAxiom)
                    , ("5 Axiom"           , fiveAxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("d Axiom"         , dAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor kfourfive
              (map (Data.Bifunctor.second pTom) posCPropTests
                ++  posModalTests
                ++  [ ("4 Axiom"          , fourAxiom)
                    , ("5 Axiom"          , fiveAxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob Axiom"         , lobAxiom)
                    , ("t Axiom"           , tAxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("d Axiom"            , dAxiom)
                    , ("B Axiom"           , bAxiom)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])

  describe "Integration tests" $ do
    describe "Equivalence between GenZ and GenT" $ modifyMaxSuccess (const 1000) $ do
      proverEqTest classical
      proverEqTest intui
      proverEqTest k
      proverEqTest kfour
      proverEqTest sfour
      proverEqTest gl
      proverEqTest t
      proverEqTest d
      proverEqTest dfour
      proverEqTest kfourfive

    describe "Proofs are at most binary" $ do
      atMostBinTest classical
      atMostBinTest intui
      atMostBinTest k
      atMostBinTest kfour
      atMostBinTest sfour
      atMostBinTest gl
      atMostBinTest t
      atMostBinTest d
      atMostBinTest dfour
      atMostBinTest kfourfive

    describe "If f and g isProvable, then Con f g isProvable" $ do
      conCheck [classical,intui]
      conCheck [k,kfour,sfour,gl,t,d,dfour,kfourfive]

    describe "If f isProvable in CPL, then neg neg f isProvable in IPL" $ do
      agreeTestTranslated classical intui (neg . neg)

    describe "Propositional tautologies in modal logics" $ modifyMaxSuccess (const 1000) $ do
      describe "K" $ do
        agreeTestTranslated classical k pTom
      describe "K4" $ do
        agreeTestTranslated classical kfour pTom
      describe "S4" $ do
        agreeTestTranslated classical sfour pTom
      describe "GL" $ do
        agreeTestTranslated classical gl pTom
      describe "T" $ do
         agreeTestTranslated classical t pTom
      describe "D" $ do
        agreeTestTranslated classical d pTom
      describe "D4" $ do
        agreeTestTranslated classical dfour pTom
      describe "K45" $ do
        agreeTestTranslated classical kfourfive pTom

    describe "f is provable in IPL iff its translation is provable in S4" $ do
      agreeTestTranslated intui sfour translation

    describe "Modal logics contain tests" $ do
      containTest k kfour
      containTest k d
      containTest d t
      containTest d dfour
      containTest t sfour
      containTest dfour sfour
      containTest kfour gl
      containTest kfour sfour
      containTest kfour kfourfive