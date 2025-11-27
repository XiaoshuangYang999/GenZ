module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

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

testsFor :: (Show f, Ord f) => Logic f -> String -> [(String,f)] -> [(String,f)] -> SpecWith ()
testsFor l description posExamples negExamples = do
  describe description $ do
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

main :: IO ()
main = hspec $ parallel $ do
  describe "Unit tests" $ do
    testsFor classical "CPL"
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
      [ ( "Bot"                , BotP)
      , ( (show contradiction) , contradiction)
      , ( (show t4)            , t4)
      , ( (show t5)            , t5)
      , ( (show t6)            , t6)
      , ( "conBotR 10"         , conBotR 10)
      , ( "conBotL 10"         , conBotL 10)
      , ( "disBotR 10"         , disBotR 10)
      , ( "disBotL 10"         , disBotL 10)
      ]

    describe "IPL.isProvableZ" $ do
      it "Top"                                                $ isProvableZ intui top
      it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ intui doubleNegationR
      it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ intui dnEM
      it (show phi)                                           $ isProvableZ intui phi
      it (show t1)                                            $ isProvableZ intui t1
      it (show t2)                                            $ isProvableZ intui t2
      it (show t3)                                            $ isProvableZ intui t3
      it "conTopR 10"                                         $ isProvableZ intui $ conTopR 10
      it "conTopL 10"                                         $ isProvableZ intui $ conTopL 10
      it "disTopR 10"                                         $ isProvableZ intui $ disTopR 10
      it "disTopL 10"                                         $ isProvableZ intui $ disTopL 10
      it "disPhiPieR 10"                                      $ isProvableZ intui $ disPhiPieR 10
      it "disPhiPieL 10"                                      $ isProvableZ intui $ disPhiPieL 10

    describe "not.IPL.isProvableZ" $ do
      it "Bot" $ not . isProvableZ intui $ BotP
      it (show contradiction)                                 $ not $ isProvableZ intui contradiction
      it ("Double negation: " ++ show doubleNegation)         $ not $ isProvableZ intui doubleNegation
      it ("Excluded middle: " ++ show excludedMiddle)         $ not $ isProvableZ intui excludedMiddle
      it ("Pierce's law: " ++ show pierce)                    $ not $ isProvableZ intui pierce
      it (show t4)                                            $ not $ isProvableZ intui t4
      it (show t5)                                            $ not $ isProvableZ intui t5
      it (show t6)                                            $ not $ isProvableZ intui t6
      it "conBotR 10"                                         $ not $ isProvableZ intui $ conBotR 10
      it "conBotL 10"                                         $ not $ isProvableZ intui $ conBotL 10
      it "disBotR 10"                                         $ not $ isProvableZ intui $ disBotR 10
      it "disBotL 10"                                         $ not $ isProvableZ intui $ disBotL 10
      it "conPieR 10"                                         $ not $ isProvableZ intui $ conPieR 10
      it "conPieL 10"                                         $ not $ isProvableZ intui $ conPieL 10
      it "disPieR 10"                                         $ not $ isProvableZ intui $ disPieR 10
      it "disPieL 10"                                         $ not $ isProvableZ intui $ disPieL 10
      it "phiImpPie 10"                                       $ not $ isProvableZ intui $ phiImpPie 10

    describe "IPL.isProvableT" $ do
      it "Top"                                                $ isProvableT intui top
      it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT intui doubleNegationR
      it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT intui dnEM
      it (show phi)                                           $ isProvableT intui phi
      it (show t1)                                            $ isProvableT intui t1
      it (show t2)                                            $ isProvableT intui t2
      it (show t3)                                            $ isProvableT intui t3
      it "conTopR 10"                                         $ isProvableT intui $ conTopR 10
      it "conTopL 10"                                         $ isProvableT intui $ conTopL 10
      it "disTopR 10"                                         $ isProvableT intui $ disTopR 10
      it "disTopL 10"                                         $ isProvableT intui $ disTopL 10
      it "disPhiPieR 10"                                      $ isProvableT intui $ disPhiPieR 10
      it "disPhiPieL 10"                                      $ isProvableT intui $ disPhiPieL 10

    describe "not.IPL.isProvableT" $ do
      it "Bot" $ not . isProvableT intui $ BotP
      it (show contradiction)                                 $ not $ isProvableT intui contradiction
      it ("Double negation: " ++ show doubleNegation)         $ not $ isProvableT intui doubleNegation
      it ("Excluded middle: " ++ show excludedMiddle)         $ not $ isProvableT intui excludedMiddle
      it ("Pierce's law: " ++ show pierce)                    $ not $ isProvableT intui pierce
      it (show t4)                                            $ not $ isProvableT intui t4
      it (show t5)                                            $ not $ isProvableT intui t5
      it (show t6)                                            $ not $ isProvableT intui t6
      it "conBotR 10"                                         $ not $ isProvableT intui $ conBotR 10
      it "conBotL 10"                                         $ not $ isProvableT intui $ conBotL 10
      it "disBotR 10"                                         $ not $ isProvableT intui $ disBotR 10
      it "disBotL 10"                                         $ not $ isProvableT intui $ disBotL 10
      it "conPieR 10"                                         $ not $ isProvableT intui $ conPieR 10
      it "conPieL 10"                                         $ not $ isProvableT intui $ conPieL 10
      it "disPieR 10"                                         $ not $ isProvableT intui $ disPieR 10
      it "disPieL 10"                                         $ not $ isProvableT intui $ disPieL 10
      it "phiImpPie 10"                                       $ not $ isProvableZ intui $ phiImpPie 10


    describe "K.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ k top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableZ k $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ k $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ k $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ k $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ k $ pTom dnEM
        it (show phi)                                           $ isProvableZ k $ pTom phi
        it (show t1)                                            $ isProvableZ k $ pTom t1
        it (show t2)                                            $ isProvableZ k $ pTom t2
        it (show t3)                                            $ isProvableZ k $ pTom t3
        it "conTopR 10"                                         $ isProvableZ k $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ k $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ k $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ k $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ k $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ k $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ k $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ k $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ k $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ k $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ k $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"      $ isProvableZ k kaxiom
        it (show f1)      $ isProvableZ k f1
        it "boxesTop 10"  $ isProvableZ k $ boxesTop 10
        it "multiVerK 10" $ isProvableZ k $ multiVerK 10

    describe "not.K.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ k BotM
        it (show contradiction) $ not $ isProvableZ k $ pTom contradiction
        it (show t4)            $ not $ isProvableZ k $ pTom t4
        it (show t5)            $ not $ isProvableZ k $ pTom t5
        it (show t6)            $ not $ isProvableZ k $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ k $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ k $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ k $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ k $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "4 axiom"            $ not $ isProvableZ k fouraxiom
        it "Lob axiom"          $ not $ isProvableZ k lobaxiom
        it "t axiom"            $ not $ isProvableZ k taxiom
        it "Consistency"        $ not $ isProvableZ k consistency
        it "Density"            $ not $ isProvableZ k density
        it "Seriality"          $ not $ isProvableZ k seriality        
        it (show f2)            $ not $ isProvableZ k f2
        it "boxesBot 10"        $ not $ isProvableZ k $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ k $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ k $ negBoxes 10
        it "lobBoxes 10"        $ not $ isProvableZ k $ lobBoxes 10
        it "boxToMoreBox 10"    $ not $ isProvableZ k $ boxToMoreBox 10
        it "boxToFewerBox 5"    $ not $ isProvableZ k $ boxToFewerBox 5

    describe "K.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT k top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableT k $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT k $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT k $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT k $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT k $ pTom dnEM
        it (show phi)                                           $ isProvableT k $ pTom phi
        it (show t1)                                            $ isProvableT k $ pTom t1
        it (show t2)                                            $ isProvableT k $ pTom t2
        it (show t3)                                            $ isProvableT k $ pTom t3
        it "conTopR 10"                                         $ isProvableT k $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT k $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT k $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT k $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT k $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT k $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT k $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT k $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT k $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT k $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT k $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"      $ isProvableT k kaxiom
        it (show f1)      $ isProvableT k f1
        it "boxesTop 10"  $ isProvableT k $ boxesTop 10
        it "multiVerK 10" $ isProvableT k $ multiVerK 10

    describe "not.K.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT k BotM
        it (show contradiction) $ not $ isProvableT k $ pTom contradiction
        it (show t4)            $ not $ isProvableT k $ pTom t4
        it (show t5)            $ not $ isProvableT k $ pTom t5
        it (show t6)            $ not $ isProvableT k $ pTom t6
        it "conBotR 10"         $ not $ isProvableT k $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT k $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT k $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT k $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "4 axiom"            $ not $ isProvableT k fouraxiom
        it "Lob axiom"          $ not $ isProvableT k lobaxiom
        it "t axiom"            $ not $ isProvableT k taxiom
        it "Consistency"        $ not $ isProvableT k consistency
        it "Density"            $ not $ isProvableT k density
        it "Seriality"          $ not $ isProvableT k seriality 
        it (show f2)            $ not $ isProvableT k f2
        it "boxesBot 10"        $ not $ isProvableT k $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT k $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT k $ negBoxes 10
        it "lobBoxes 10"        $ not $ isProvableT k $ lobBoxes 10
        it "boxToMoreBox 10"    $ not $ isProvableT k $ boxToMoreBox 10
        it "boxToFewerBox 5"    $ not $ isProvableT k $ boxToFewerBox 5


    describe "K4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ kfour top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableZ kfour$ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ kfour $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ kfour $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ kfour $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ kfour $ pTom dnEM
        it (show phi)                                           $ isProvableZ kfour $ pTom phi
        it (show t1)                                            $ isProvableZ kfour $ pTom t1
        it (show t2)                                            $ isProvableZ kfour $ pTom t2
        it (show t3)                                            $ isProvableZ kfour $ pTom t3
        it "conTopR 10"                                         $ isProvableZ kfour $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ kfour $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ kfour $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ kfour $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ kfour $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ kfour $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ kfour $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ kfour $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ kfour $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ kfour $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ kfour $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableZ kfour kaxiom
        it "4 axiom"           $ isProvableZ kfour fouraxiom
        it (show f1)           $ isProvableZ kfour f1
        it "boxesTop 10"       $ isProvableZ kfour $ boxesTop 10
        it "multiVerK 10"      $ isProvableZ kfour $ multiVerK 10
        it "boxToMoreBox 10"   $ isProvableZ kfour $ boxToMoreBox 10

    describe "not.K4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ kfour BotM
        it (show contradiction) $ not $ isProvableZ kfour $ pTom contradiction
        it (show t4)            $ not $ isProvableZ kfour $ pTom t4
        it (show t5)            $ not $ isProvableZ kfour $ pTom t5
        it (show t6)            $ not $ isProvableZ kfour $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ kfour $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ kfour $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ kfour $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ kfour $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableZ kfour lobaxiom
        it "t axiom"            $ not $ isProvableZ kfour taxiom
        it "Consistency"        $ not $ isProvableZ kfour consistency
        it "Density"            $ not $ isProvableZ kfour density
        it "Seriality"          $ not $ isProvableZ kfour seriality 
        it (show f2)            $ not $ isProvableZ kfour f2
        it "boxesBot 10"        $ not $ isProvableZ kfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ kfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ kfour $ negBoxes 10
        it "lobBoxes 10"        $ not $ isProvableZ kfour $ lobBoxes 10
        it "boxToFewerBox 5"    $ not $ isProvableZ kfour $ boxToFewerBox 5

    describe "K4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT kfour top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableT kfour$ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT kfour $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT kfour $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT kfour $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT kfour $ pTom dnEM
        it (show phi)                                           $ isProvableT kfour $ pTom phi
        it (show t1)                                            $ isProvableT kfour $ pTom t1
        it (show t2)                                            $ isProvableT kfour $ pTom t2
        it (show t3)                                            $ isProvableT kfour $ pTom t3
        it "conTopR 10"                                         $ isProvableT kfour $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT kfour $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT kfour $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT kfour $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT kfour $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT kfour $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT kfour $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT kfour $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT kfour $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT kfour $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT kfour $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableT kfour kaxiom
        it "4 axiom"           $ isProvableT kfour fouraxiom
        it (show f1)           $ isProvableT kfour f1
        it "boxesTop 10"       $ isProvableT kfour $ boxesTop 10
        it "multiVerK 10"      $ isProvableT kfour $ multiVerK 10
        it "boxToMoreBox 10"   $ isProvableT kfour $ boxToMoreBox 10

    describe "not.K4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT kfour BotM
        it (show contradiction) $ not $ isProvableT kfour $ pTom contradiction
        it (show t4)            $ not $ isProvableT kfour $ pTom t4
        it (show t5)            $ not $ isProvableT kfour $ pTom t5
        it (show t6)            $ not $ isProvableT kfour $ pTom t6
        it "conBotR 10"         $ not $ isProvableT kfour $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT kfour $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT kfour $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT kfour $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableT kfour lobaxiom
        it "t axiom"            $ not $ isProvableT kfour taxiom
        it "Consistency"        $ not $ isProvableT kfour consistency
        it "Density"            $ not $ isProvableT kfour density
        it "Seriality"          $ not $ isProvableT kfour seriality 
        it (show f2)            $ not $ isProvableT kfour f2
        it "boxesBot 10"        $ not $ isProvableT kfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT kfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT kfour $ negBoxes 10
        it "lobBoxes 10"        $ not $ isProvableT kfour $ lobBoxes 10
        it "boxToFewerBox 5"    $ not $ isProvableT kfour $ boxToFewerBox 5

  -- New tests for T
    describe "T.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ t $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ t $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ t $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ t $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ t $ pTom dnEM
        it (show phi)                                           $ isProvableZ t $ pTom phi
        it (show t1)                                            $ isProvableZ t $ pTom t1
        it (show t2)                                            $ isProvableZ t $ pTom t2
        it (show t3)                                            $ isProvableZ t $ pTom t3
        it "conTopR 10"                                         $ isProvableZ t $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ t $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ t $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ t $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ t $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ t $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ t $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ t $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ t $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ t $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ t $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableZ t kaxiom
        it "t axiom"           $ isProvableZ t taxiom
        it "Consistency"       $ isProvableZ t consistency
        it "Density"           $ isProvableZ t density
        it "Seriality"         $ isProvableZ t seriality
        it (show f1)           $ isProvableZ t f1
        it "boxesTop 10"       $ isProvableZ t $ boxesTop 10
        it "multiVerK 10"      $ isProvableZ t $ multiVerK 10
        it "boxToFewerBox 10"   $ isProvableZ t $ boxToFewerBox 10
    describe "not.T.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ t BotM
        it (show contradiction) $ not $ isProvableZ t $ pTom contradiction
        it (show t4)            $ not $ isProvableZ t $ pTom t4
        it (show t5)            $ not $ isProvableZ t $ pTom t5
        it (show t6)            $ not $ isProvableZ t $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ t $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ t $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ t $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ t $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableZ t lobaxiom
        it "4 axiom"            $ not $ isProvableZ t fouraxiom
        it (show f2)            $ not $ isProvableZ t f2
        it "boxesBot 10"        $ not $ isProvableZ t $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ t $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ t $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableZ t $ lobBoxes 10
        it "boxToMoreBox 5"    $ not $ isProvableZ t $ boxToMoreBox 5
    describe "T.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT t $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT t $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT t $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT t $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT t $ pTom dnEM
        it (show phi)                                           $ isProvableT t $ pTom phi
        it (show t1)                                            $ isProvableT t $ pTom t1
        it (show t2)                                            $ isProvableT t $ pTom t2
        it (show t3)                                            $ isProvableT t $ pTom t3
        it "conTopR 10"                                         $ isProvableT t $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT t $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT t $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT t $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT t $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT t $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT t $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT t $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT t $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT t $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT t $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableT t kaxiom
        it "t axiom"           $ isProvableT t taxiom
        it "Consistency"       $ isProvableT t consistency
        it "Density"           $ isProvableT t density
        it "Seriality"         $ isProvableZ t seriality
        it (show f1)           $ isProvableT t f1
        it "boxesTop 10"       $ isProvableT t $ boxesTop 10
        it "multiVerK 10"      $ isProvableT t $ multiVerK 10
        it "boxToFewerBox 10"   $ isProvableT t $ boxToFewerBox 10
    describe "not.T.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT t BotM
        it (show contradiction) $ not $ isProvableT t $ pTom contradiction
        it (show t4)            $ not $ isProvableT t $ pTom t4
        it (show t5)            $ not $ isProvableT t $ pTom t5
        it (show t6)            $ not $ isProvableT t $ pTom t6
        it "conBotR 10"         $ not $ isProvableT t $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT t $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT t $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT t $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableT t lobaxiom
        it "4 axiom"            $ not $ isProvableT t fouraxiom
        it (show f2)            $ not $ isProvableT t f2
        it "boxesBot 10"        $ not $ isProvableT t $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT t $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT t $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableT t $ lobBoxes 10
        it "boxToMoreBox 5"    $ not $ isProvableT t $ boxToMoreBox 5

-- New tests for D  
    describe "D.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ d $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ d $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ d $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ d $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ d $ pTom dnEM
        it (show phi)                                           $ isProvableZ d $ pTom phi
        it (show t1)                                            $ isProvableZ d $ pTom t1
        it (show t2)                                            $ isProvableZ d $ pTom t2
        it (show t3)                                            $ isProvableZ d $ pTom t3
        it "conTopR 10"                                         $ isProvableZ d $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ d $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ d $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ d $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ d $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ d $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ d $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ d $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ d $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ d $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ d $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableZ d kaxiom        
        it "Consistency"       $ isProvableZ d consistency
        it "Seriality"         $ isProvableZ d seriality
        it (show f1)           $ isProvableZ d f1
        it "boxesTop 10"       $ isProvableZ d $ boxesTop 10
        it "multiVerK 10"      $ isProvableZ d $ multiVerK 10
    describe "not.D.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ d BotM
        it (show contradiction) $ not $ isProvableZ d $ pTom contradiction
        it (show t4)            $ not $ isProvableZ d $ pTom t4
        it (show t5)            $ not $ isProvableZ d $ pTom t5
        it (show t6)            $ not $ isProvableZ d $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ d $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ d $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ d $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ d $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableZ d lobaxiom
        it "t axiom"            $ not $ isProvableZ d taxiom
        it "4 axiom"            $ not $ isProvableZ d fouraxiom
        it "Density"            $ not $ isProvableZ d density
        it (show f2)            $ not $ isProvableZ d f2
        it "boxesBot 10"        $ not $ isProvableZ d $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ d $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ d $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableZ d $ lobBoxes 10
        it "boxToMoreBox 5"     $ not $ isProvableZ d $ boxToMoreBox 5
        it "boxToFewerBox 10"   $ not $ isProvableZ d $ boxToFewerBox 5
    describe "D.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT d $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT d $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT d $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT d $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT d $ pTom dnEM
        it (show phi)                                           $ isProvableT d $ pTom phi
        it (show t1)                                            $ isProvableT d $ pTom t1
        it (show t2)                                            $ isProvableT d $ pTom t2
        it (show t3)                                            $ isProvableT d $ pTom t3
        it "conTopR 10"                                         $ isProvableT d $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT d $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT d $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT d $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT d $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT d $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT d $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT d $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT d $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT d $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT d $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableT d kaxiom        
        it "Consistency"       $ isProvableT d consistency
        it "Seriality"         $ isProvableT d seriality
        it (show f1)           $ isProvableT d f1
        it "boxesTop 10"       $ isProvableT d $ boxesTop 10
        it "multiVerK 10"      $ isProvableT d $ multiVerK 10
    describe "not.D.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT d BotM
        it (show contradiction) $ not $ isProvableT d $ pTom contradiction
        it (show t4)            $ not $ isProvableT d $ pTom t4
        it (show t5)            $ not $ isProvableT d $ pTom t5
        it (show t6)            $ not $ isProvableT d $ pTom t6
        it "conBotR 10"         $ not $ isProvableT d $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT d $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT d $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT d $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableT d lobaxiom
        it "t axiom"            $ not $ isProvableT d taxiom
        it "4 axiom"            $ not $ isProvableT d fouraxiom
        it "Density"            $ not $ isProvableT d density
        it (show f2)            $ not $ isProvableT d f2
        it "boxesBot 10"        $ not $ isProvableT d $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT d $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT d $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableT d $ lobBoxes 10
        it "boxToMoreBox 5"     $ not $ isProvableT d $ boxToMoreBox 5
        it "boxToFewerBox 10"   $ not $ isProvableZ d $ boxToFewerBox 5

-- New tests for D4  
    describe "D4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ dfour $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ dfour $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ dfour $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ dfour $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ dfour $ pTom dnEM
        it (show phi)                                           $ isProvableZ dfour $ pTom phi
        it (show t1)                                            $ isProvableZ dfour $ pTom t1
        it (show t2)                                            $ isProvableZ dfour $ pTom t2
        it (show t3)                                            $ isProvableZ dfour $ pTom t3
        it "conTopR 10"                                         $ isProvableZ dfour $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ dfour $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ dfour $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ dfour $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ dfour $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ dfour $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ dfour $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ dfour $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ dfour $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ dfour $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ dfour $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableZ dfour kaxiom 
        it "4 axiom"           $ isProvableZ dfour fouraxiom      
        it "Consistency"       $ isProvableZ dfour consistency
        it "Seriality"         $ isProvableZ dfour seriality
        it (show f1)           $ isProvableZ dfour f1
        it "boxesTop 10"       $ isProvableZ dfour $ boxesTop 10
        it "multiVerK 10"      $ isProvableZ dfour $ multiVerK 10
        it "boxToMoreBox 5"    $ isProvableZ dfour $ boxToMoreBox 5
    describe "not.D4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ dfour BotM
        it (show contradiction) $ not $ isProvableZ dfour $ pTom contradiction
        it (show t4)            $ not $ isProvableZ dfour $ pTom t4
        it (show t5)            $ not $ isProvableZ dfour $ pTom t5
        it (show t6)            $ not $ isProvableZ dfour $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ dfour $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ dfour $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ dfour $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ dfour $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableZ dfour lobaxiom
        it "t axiom"            $ not $ isProvableZ dfour taxiom
        it "Density"            $ not $ isProvableZ dfour density
        it (show f2)            $ not $ isProvableZ dfour f2
        it "boxesBot 10"        $ not $ isProvableZ dfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ dfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ dfour $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableZ dfour $ lobBoxes 10
        it "boxToFewerBox 10"   $ not $ isProvableZ dfour $ boxToFewerBox 5
    describe "D4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT dfour $ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT dfour $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT dfour $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT dfour $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT dfour $ pTom dnEM
        it (show phi)                                           $ isProvableT dfour $ pTom phi
        it (show t1)                                            $ isProvableT dfour $ pTom t1
        it (show t2)                                            $ isProvableT dfour $ pTom t2
        it (show t3)                                            $ isProvableT dfour $ pTom t3
        it "conTopR 10"                                         $ isProvableT dfour $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT dfour $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT dfour $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT dfour $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT dfour $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT dfour $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT dfour $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT dfour $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT dfour $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT dfour $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT dfour $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableT dfour kaxiom 
        it "4 axiom"           $ isProvableT dfour fouraxiom      
        it "Consistency"       $ isProvableT dfour consistency
        it "Seriality"         $ isProvableT dfour seriality
        it (show f1)           $ isProvableT dfour f1
        it "boxesTop 10"       $ isProvableT dfour $ boxesTop 10
        it "multiVerK 10"      $ isProvableT dfour $ multiVerK 10
        it "boxToMoreBox 5"    $ isProvableT dfour $ boxToMoreBox 5
    describe "not.D4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT dfour BotM
        it (show contradiction) $ not $ isProvableT dfour $ pTom contradiction
        it (show t4)            $ not $ isProvableT dfour $ pTom t4
        it (show t5)            $ not $ isProvableT dfour $ pTom t5
        it (show t6)            $ not $ isProvableT dfour $ pTom t6
        it "conBotR 10"         $ not $ isProvableT dfour $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT dfour $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT dfour $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT dfour $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableT dfour lobaxiom
        it "t axiom"            $ not $ isProvableT dfour taxiom
        it "Density"            $ not $ isProvableT dfour density
        it (show f2)            $ not $ isProvableT dfour f2
        it "boxesBot 10"        $ not $ isProvableT dfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT dfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT dfour $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableT dfour $ lobBoxes 10
        it "boxToFewerBox 10"   $ not $ isProvableT dfour $ boxToFewerBox 5
-- S4
    describe "S4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ sfour top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableZ sfour$ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ sfour $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ sfour $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ sfour $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ sfour $ pTom dnEM
        it (show phi)                                           $ isProvableZ sfour $ pTom phi
        it (show t1)                                            $ isProvableZ sfour $ pTom t1
        it (show t2)                                            $ isProvableZ sfour $ pTom t2
        it (show t3)                                            $ isProvableZ sfour $ pTom t3
        it "conTopR 10"                                         $ isProvableZ sfour $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ sfour $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ sfour $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ sfour $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ sfour $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ sfour $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ sfour $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ sfour $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ sfour $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ sfour $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ sfour $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"            $ isProvableZ sfour kaxiom
        it "4 axiom"            $ isProvableZ sfour fouraxiom
        it "t axiom"            $ isProvableZ sfour taxiom
        it "Consistency"        $ isProvableZ sfour consistency
        it "Density"            $ isProvableZ sfour density
        it "Seriality"         $ isProvableZ sfour seriality
        it (show f1)            $ isProvableZ sfour f1
        it "boxesTop 10"        $ isProvableZ sfour $ boxesTop 10
        it "multiVerK 10"       $ isProvableZ sfour $ multiVerK 10
        it "boxToMoreBox 10"    $ isProvableZ sfour $ boxToMoreBox 10
        it "boxToFewerBox 5"    $ isProvableZ sfour $ boxToFewerBox 5

    describe "not.S4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ sfour BotM
        it (show contradiction) $ not $ isProvableZ sfour $ pTom contradiction
        it (show t4)            $ not $ isProvableZ sfour $ pTom t4
        it (show t5)            $ not $ isProvableZ sfour $ pTom t5
        it (show t6)            $ not $ isProvableZ sfour $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ sfour $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ sfour $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ sfour $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ sfour $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableZ sfour lobaxiom
        it (show f2)            $ not $ isProvableZ sfour f2
        it "boxesBot 10"        $ not $ isProvableZ sfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ sfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ sfour $ negBoxes 10
        it "lobBoxes 10"        $ not $ isProvableZ sfour $ lobBoxes 10

    describe "S4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT sfour top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableT sfour$ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT sfour $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT sfour $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT sfour $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT sfour $ pTom dnEM
        it (show phi)                                           $ isProvableT sfour $ pTom phi
        it (show t1)                                            $ isProvableT sfour $ pTom t1
        it (show t2)                                            $ isProvableT sfour $ pTom t2
        it (show t3)                                            $ isProvableT sfour $ pTom t3
        it "conTopR 10"                                         $ isProvableT sfour $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT sfour $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT sfour $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT sfour $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT sfour $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT sfour $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT sfour $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT sfour $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT sfour $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT sfour $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT sfour $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"            $ isProvableT sfour kaxiom
        it "4 axiom"            $ isProvableT sfour fouraxiom
        it "t axiom"            $ isProvableT sfour taxiom
        it "Consistency"        $ isProvableT sfour consistency
        it "Density"            $ isProvableT sfour density
        it "Seriality"          $ isProvableT sfour seriality
        it (show f1)            $ isProvableT sfour f1
        it "boxesTop 10"        $ isProvableT sfour $ boxesTop 10
        it "multiVerK 10"       $ isProvableT sfour $ multiVerK 10
        it "boxToMoreBox 10"    $ isProvableT sfour $ boxToMoreBox 10
        it "boxToFewerBox 5"    $ isProvableT sfour $ boxToFewerBox 5

    describe "not.S4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT sfour BotM
        it (show contradiction) $ not $ isProvableT sfour $ pTom contradiction
        it (show t4)            $ not $ isProvableT sfour $ pTom t4
        it (show t5)            $ not $ isProvableT sfour $ pTom t5
        it (show t6)            $ not $ isProvableT sfour $ pTom t6
        it "conBotR 10"         $ not $ isProvableT sfour $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT sfour $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT sfour $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT sfour $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "Lob axiom"          $ not $ isProvableT sfour lobaxiom
        it (show f2)            $ not $ isProvableT sfour f2
        it "boxesBot 10"        $ not $ isProvableT sfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT sfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT sfour $ negBoxes 10
        it "lobBoxes 10"        $ not $ isProvableT sfour $ lobBoxes 10


    describe "GL.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ gl top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableZ gl$ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ gl $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ gl $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableZ gl $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ gl $ pTom dnEM
        it (show phi)                                           $ isProvableZ gl $ pTom phi
        it (show t1)                                            $ isProvableZ gl $ pTom t1
        it (show t2)                                            $ isProvableZ gl $ pTom t2
        it (show t3)                                            $ isProvableZ gl $ pTom t3
        it "conTopR 10"                                         $ isProvableZ gl $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableZ gl $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableZ gl $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableZ gl $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableZ gl $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableZ gl $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableZ gl $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableZ gl $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableZ gl $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableZ gl $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableZ gl $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableZ gl kaxiom
        it "4 axiom"           $ isProvableZ gl fouraxiom
        it "Lob axiom"         $ isProvableZ gl lobaxiom
        it (show f1)           $ isProvableZ gl f1
        it "boxesTop 10"       $ isProvableZ gl $ boxesTop 10
        it "multiVerK 10"      $ isProvableZ gl $ multiVerK 10
        it "boxToMoreBox 10"   $ isProvableZ gl $ boxToMoreBox 10
        it "lobBoxes 10"       $ isProvableZ gl $ lobBoxes 10

    describe "not.GL.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableZ gl BotM
        it (show contradiction) $ not $ isProvableZ gl $ pTom contradiction
        it (show t4)            $ not $ isProvableZ gl $ pTom t4
        it (show t5)            $ not $ isProvableZ gl $ pTom t5
        it (show t6)            $ not $ isProvableZ gl $ pTom t6
        it "conBotR 10"         $ not $ isProvableZ gl $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableZ gl $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableZ gl $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableZ gl $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "t axiom"            $ not $ isProvableZ gl taxiom
        it "Consistency"        $ not $ isProvableZ gl consistency
        it "Density"            $ not $ isProvableZ gl density
        it "Seriality"          $ not $ isProvableZ gl seriality 
        it (show f2)            $ not $ isProvableZ gl f2
        it "boxesBot 10"        $ not $ isProvableZ gl $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ gl $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ gl $ negBoxes 10
        it "boxToFewerBox 5"    $ not $ isProvableZ gl $ boxToFewerBox 5

    describe "GL.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT gl top
        it ("Double negation: " ++ show doubleNegation)         $ isProvableT gl$ pTom doubleNegation
        it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT gl $ pTom doubleNegationR
        it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT gl $ pTom excludedMiddle
        it ("Pierce's law: " ++ show pierce)                    $ isProvableT gl $ pTom pierce
        it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT gl $ pTom dnEM
        it (show phi)                                           $ isProvableT gl $ pTom phi
        it (show t1)                                            $ isProvableT gl $ pTom t1
        it (show t2)                                            $ isProvableT gl $ pTom t2
        it (show t3)                                            $ isProvableT gl $ pTom t3
        it "conTopR 10"                                         $ isProvableT gl $ pTom $ conTopR 10
        it "conTopL 10"                                         $ isProvableT gl $ pTom $ conTopL 10
        it "disTopR 10"                                         $ isProvableT gl $ pTom $ disTopR 10
        it "disTopL 10"                                         $ isProvableT gl $ pTom $ disTopL 10
        it "conPieR 10"                                         $ isProvableT gl $ pTom $ conPieR 10
        it "conPieL 10"                                         $ isProvableT gl $ pTom $ conPieL 10
        it "disPieR 10"                                         $ isProvableT gl $ pTom $ disPieR 10
        it "disPieL 10"                                         $ isProvableT gl $ pTom $ disPieL 10
        it "disPhiPieR 10"                                      $ isProvableT gl $ pTom $ disPhiPieR 10
        it "disPhiPieL 10"                                      $ isProvableT gl $ pTom $ disPhiPieL 10
        it "phiImpPie 10"                                       $ isProvableT gl $ pTom $ phiImpPie 10
      describe "Modal formulas" $ do
        it "k axiom"           $ isProvableT gl kaxiom
        it "4 axiom"           $ isProvableT gl fouraxiom
        it "Lob axiom"         $ isProvableT gl lobaxiom
        it (show f1)           $ isProvableT gl f1
        it "boxesTop 10"       $ isProvableT gl $ boxesTop 10
        it "multiVerK 10"      $ isProvableT gl $ multiVerK 10
        it "boxToMoreBox 10"   $ isProvableT gl $ boxToMoreBox 10
        it "lobBoxes 10"       $ isProvableT gl $ lobBoxes 10

    describe "not.GL.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Bot"                $ not $ isProvableT gl BotM
        it (show contradiction) $ not $ isProvableT gl $ pTom contradiction
        it (show t4)            $ not $ isProvableT gl $ pTom t4
        it (show t5)            $ not $ isProvableT gl $ pTom t5
        it (show t6)            $ not $ isProvableT gl $ pTom t6
        it "conBotR 10"         $ not $ isProvableT gl $ pTom $ conBotR 10
        it "conBotL 10"         $ not $ isProvableT gl $ pTom $ conBotL 10
        it "disBotR 10"         $ not $ isProvableT gl $ pTom $ disBotR 10
        it "disBotL 10"         $ not $ isProvableT gl $ pTom $ disBotL 10
      describe "Modal formulas" $ do
        it "t axiom"            $ not $ isProvableT gl taxiom
        it "Consistency"        $ not $ isProvableT gl consistency
        it "Density"            $ not $ isProvableT gl density        
        it "Seriality"          $ not $ isProvableZ gl seriality
        it (show f2)            $ not $ isProvableT gl f2
        it "boxesBot 10"        $ not $ isProvableT gl $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT gl $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT gl $ negBoxes 10
        it "boxToFewerBox 5"    $ not $ isProvableT gl $ boxToFewerBox 5

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
