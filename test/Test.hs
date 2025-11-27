module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Data.Bifunctor
import General
import FormP
import FormM
import CPL
import IPL
import K
import K4
import GL
import S4
import T
import D
import D4

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

-- | Set a time limit.
-- Test cases will be discarded if they take more than 5 seconds.
limit :: Int
limit = 5 * 1000000 -- in microseconds

-- | Ensure laziness
implies :: Bool -> Bool -> Bool
implies x = if x then id else const True

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

posModalTests :: [(String, FormM)]
posModalTests = 
      [ ("k axiom"          , kaxiom)                   
      , (show f1            , f1)
      , ("boxesTop 10"      , boxesTop 10)
      , ("multiVerK 10"     , multiVerK 10)
      ]

negModalTests :: [(String, FormM)]
negModalTests =
      [ (show f2            , f2)
      , ("negBoxes 10"      , negBoxes 10)
      , ("boxesBot 10"      , boxesBot 10)
      , ("extraAtK 10"      , extraAtK 10)]

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
                ++  [ ("4 axiom"           , fouraxiom)
                    , ("Lob axiom"         , lobaxiom)
                    , ("t axiom"           , taxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("Seriality"         , seriality)
                    , ("lobBoxes 5"      , lobBoxes 5)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("boxToFewerBox 5"  , boxToFewerBox 5)
                    ])
    testsFor kfour 
              (map (Data.Bifunctor.second pTom) posCPropTests 
                ++  posModalTests
                ++  [ ("4 axiom"          , fouraxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob axiom"         , lobaxiom)
                    , ("t axiom"           , taxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("Seriality"         , seriality)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor t 
              (map (Data.Bifunctor.second pTom) posCPropTests 
                ++  posModalTests
                ++  [ ("t axiom"           , taxiom)                    
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("Seriality"         , seriality)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob axiom"         , lobaxiom)
                    , ("4 axiom"          , fouraxiom)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
    testsFor d 
              (map (Data.Bifunctor.second pTom) posCPropTests 
                ++  posModalTests
                ++  [ ("Consistency"       , consistency)
                    , ("Seriality"         , seriality)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob axiom"         , lobaxiom)
                    , ("t axiom"           , taxiom)
                    , ("4 axiom"          , fouraxiom)  
                    , ("Density"           , density)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor dfour 
              (map (Data.Bifunctor.second pTom) posCPropTests 
                ++  posModalTests
                ++  [ ("4 axiom"          , fouraxiom) 
                    , ("Consistency"       , consistency)
                    , ("Seriality"         , seriality)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob axiom"         , lobaxiom)
                    , ("t axiom"           , taxiom) 
                    , ("Density"           , density)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
    testsFor sfour 
              (map (Data.Bifunctor.second pTom) posCPropTests 
                ++  posModalTests
                ++  [ ("4 axiom"          , fouraxiom)
                    , ("t axiom"           , taxiom) 
                    , ("Density"           , density) 
                    , ("Consistency"       , consistency)
                    , ("Seriality"         , seriality)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("Lob axiom"         , lobaxiom)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    ])
    testsFor gl 
              (map (Data.Bifunctor.second pTom) posCPropTests 
                ++  posModalTests
                ++  [ ("4 axiom"          , fouraxiom)
                    , ("Lob axiom"         , lobaxiom)
                    , ("boxToMoreBox 5"  , boxToMoreBox 5)
                    , ("lobBoxes 5"       , lobBoxes 5)
                    ])
              (map (Data.Bifunctor.second pTom) negCPropTests
                ++  negModalTests
                ++  [ ("t axiom"           , taxiom)
                    , ("Consistency"       , consistency)
                    , ("Density"           , density)
                    , ("Seriality"         , seriality)
                    , ("boxToFewerBox 5"   , boxToFewerBox 5)
                    ])

  describe "Integration tests" $ do
    describe "Equivalence between GenZ and GenT" $ modifyMaxSuccess (const 1000) $ do
      prop "In CPL" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableT classical f
      prop "In IPL" $
        \ f -> discardAfter limit $ isProvableZ intui f === isProvableT intui f
      prop "In K" $
        \ f -> discardAfter limit $ isProvableZ k f === isProvableT k f
      prop "In K4" $
        \ f -> discardAfter limit $ isProvableZ kfour f === isProvableT kfour f
      prop "In S4" $
        \ f -> discardAfter limit $ isProvableZ sfour f === isProvableT sfour f
      prop "In GL" $
        \ f -> discardAfter limit $ isProvableZ gl f === isProvableT gl f
      prop "In T" $
        \ f -> discardAfter limit $ isProvableZ t f === isProvableT t f
      prop "In D" $
        \ f -> discardAfter limit $ isProvableZ d f === isProvableT d f
      prop "In D4" $
        \ f -> discardAfter limit $ isProvableZ dfour f === isProvableT dfour f

    describe "Proofs are at most binary" $ do
        let hasLeqTwoChildren (Node _ Nothing _) = True
            hasLeqTwoChildren (Node _ (Just (_, ts)) _ ) = length ts <= 2 && all hasLeqTwoChildren ts
        prop "GenZ for CPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ classical f
        prop "GenT for CPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT classical f
        prop "GenZ for IPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ intui f
        prop "GenT for IPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT intui f
        prop "GenZ for K" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ k f
        prop "GenT for K" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT k f
        prop "GenZ for K4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ kfour f
        prop "GenT for K4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT kfour f
        prop "GenZ for S4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ sfour f
        prop "GenT for S4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT sfour f
        prop "GenZ for GL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ gl f
        prop "GenT for GL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT gl f
        prop "GenZ for T" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ t f
        prop "GenT for T" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT t f
        prop "GenZ for D" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ d f
        prop "GenT for D" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT d f
        prop "GenZ for D4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ dfour f
        prop "GenT for D4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT dfour f

    conCheck [("CPL", classical), ("IPL", intui)]

    conCheck [("K", k), ("K4", kfour), ("S4",sfour), ("GL",gl), ("T", t), ("D", d), ("D4", dfour)]

    agreeTestTranslated classical intui (neg . neg)
    agreeTestTranslated classical k pTom
    agreeTestTranslated classical kfour pTom
    agreeTestTranslated classical sfour pTom
    agreeTestTranslated classical gl pTom
    agreeTestTranslated classical t pTom
    agreeTestTranslated classical d pTom
    agreeTestTranslated classical dfour pTom

    containTest k kfour
    containTest k sfour
    containTest kfour gl
    containTest t sfour
    containTest k d
    containTest d t
    containTest d dfour
    containTest dfour sfour

    agreeTestTranslated intui sfour translation

conCheck :: (Arbitrary f, Show f, Ord f, PropLog f) => [(String, Logic f)] -> SpecWith ()
conCheck = mapM_ $ \ (s,l) -> do
  prop ("GenZ for " ++ s) $
    \ f g -> discardAfter limit $ (isProvableZ l f && isProvableZ l g) `implies` isProvableZ l (con f g)
  prop ("GenT for " ++ s) $
    \ f g -> discardAfter limit $ (isProvableT l f && isProvableT l g) `implies` isProvableT l (con f g)

containTest :: (Arbitrary f, Show f, Ord f, PropLog f) => Logic f -> Logic f -> SpecWith ()
containTest l1 l2 = do
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
