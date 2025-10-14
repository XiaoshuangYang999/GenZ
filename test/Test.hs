module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import General
import CPL
import IPL
import K
import K4
import GL
import S4
import PForm
import MForm

main :: IO ()
main = hspec $ do
  describe "Unit tests" $ do
    describe "CPL.isProvableZ" $ do
      it "Top"                                                $ isProvableZ classical topP
      it ("Double negation: " ++ show doubleNegation)         $ isProvableZ classical doubleNegation
      it ("Double negation right: " ++ show doubleNegationR)  $ isProvableZ classical doubleNegationR
      it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableZ classical excludedMiddle
      it ("Pierce's law: " ++ show pierce)                    $ isProvableZ classical pierce
      it ("Double negation of excluded middle " ++ show dnEM) $ isProvableZ classical dnEM
      it (show phi)                                           $ isProvableZ classical phi
      it (show t1)                                            $ isProvableZ classical t1
      it (show t2)                                            $ isProvableZ classical t2
      it (show t3)                                            $ isProvableZ classical t3
      it "conTopR 10"                                         $ isProvableZ classical $ conTopR 10
      it "conTopL 10"                                         $ isProvableZ classical $ conTopL 10
      it "disTopR 10"                                         $ isProvableZ classical $ disTopR 10
      it "disTopL 10"                                         $ isProvableZ classical $ disTopL 10
      it "conPieR 10"                                         $ isProvableZ classical $ conPieR 10
      it "conPieL 10"                                         $ isProvableZ classical $ conPieL 10
      it "disPieR 10"                                         $ isProvableZ classical $ disPieR 10
      it "disPieL 10"                                         $ isProvableZ classical $ disPieL 10
      it "disPhiPieR 10"                                      $ isProvableZ classical $ disPhiPieR 10
      it "disPhiPieL 10"                                      $ isProvableZ classical $ disPhiPieL 10
      it "phiImpPie 10"                                       $ isProvableZ classical $ phiImpPie 10

    describe "not.CPL.isProvableZ" $ do
      it "Bot"                $ not . isProvableZ classical $ BotP
      it (show contradiction) $ not $ isProvableZ classical contradiction
      it (show t4)            $ not $ isProvableZ classical t4
      it (show t5)            $ not $ isProvableZ classical t5
      it (show t6)            $ not $ isProvableZ classical t6
      it "conBotR 10"         $ not $ isProvableZ classical $ conBotR 10
      it "conBotL 10"         $ not $ isProvableZ classical $ conBotL 10
      it "disBotR 10"         $ not $ isProvableZ classical $ disBotR 10
      it "disBotL 10"         $ not $ isProvableZ classical $ disBotL 10

    describe "CPL.isProvableT" $ do
      it "Top"                                                $ isProvableT classical topP
      it ("Double negation: " ++ show doubleNegation)         $ isProvableT classical doubleNegation
      it ("Double negation right: " ++ show doubleNegationR)  $ isProvableT classical doubleNegationR
      it ("Excluded middle: " ++ show excludedMiddle)         $ isProvableT classical excludedMiddle
      it ("Pierce's law: " ++ show pierce)                    $ isProvableT classical pierce
      it ("Double negation of excluded middle " ++ show dnEM) $ isProvableT classical dnEM
      it (show phi)                                           $ isProvableT classical phi
      it (show t1)                                            $ isProvableT classical t1
      it (show t2)                                            $ isProvableT classical t2
      it (show t3)                                            $ isProvableT classical t3
      it "conTopR 10"                                         $ isProvableT classical $ conTopR 10
      it "conTopL 10"                                         $ isProvableT classical $ conTopL 10
      it "disTopR 10"                                         $ isProvableT classical $ disTopR 10
      it "disTopL 10"                                         $ isProvableT classical $ disTopL 10
      it "conPieR 10"                                         $ isProvableT classical $ conPieR 10
      it "conPieL 10"                                         $ isProvableT classical $ conPieL 10
      it "disPieR 10"                                         $ isProvableT classical $ disPieR 10
      it "disPieL 10"                                         $ isProvableT classical $ disPieL 10
      it "disPhiPieR 10"                                      $ isProvableT classical $ disPhiPieR 10
      it "disPhiPieL 10"                                      $ isProvableT classical $ disPhiPieL 10
      it "phiImpPie 10"                                       $ isProvableT classical $ phiImpPie 10

    describe "not.CPL.isProvableT" $ do
      it "Bot"                $ not . isProvableT classical $ BotP
      it (show contradiction) $ not $ isProvableT classical contradiction
      it (show t4)            $ not $ isProvableT classical t4
      it (show t5)            $ not $ isProvableT classical t5
      it (show t6)            $ not $ isProvableT classical t6
      it "conBotR 10"         $ not $ isProvableT classical $ conBotR 10
      it "conBotL 10"         $ not $ isProvableT classical $ conBotL 10
      it "disBotR 10"         $ not $ isProvableT classical $ disBotR 10
      it "disBotL 10"         $ not $ isProvableZ classical $ disBotL 10


    describe "IPL.isProvableZ" $ do
      it "Top"                                                $ isProvableZ intui topP
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
      it "Top"                                                $ isProvableT intui topP
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
        it "Top"                                                $ isProvableZ k topM
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
        it (show f2)            $ not $ isProvableZ k f2
        it "boxesBot 10"        $ not $ isProvableZ k $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ k $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ k $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableZ k $ lobBoxes 10
        it "boxToMoreBox 10"    $ not $ isProvableZ k $ boxToMoreBox 10
        it "boxToFewerBox 5"    $ not $ isProvableZ k $ boxToFewerBox 5

    describe "K.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT k topM
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
        it (show f2)            $ not $ isProvableT k f2
        it "boxesBot 10"        $ not $ isProvableT k $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT k $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT k $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableT k $ lobBoxes 10
        it "boxToMoreBox 10"    $ not $ isProvableT k $ boxToMoreBox 10
        it "boxToFewerBox 5"    $ not $ isProvableT k $ boxToFewerBox 5


    describe "K4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ kfour topM
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
        it (show f2)            $ not $ isProvableZ kfour f2
        it "boxesBot 10"        $ not $ isProvableZ kfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ kfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ kfour $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableZ kfour $ lobBoxes 10
        it "boxToFewerBox 5"    $ not $ isProvableZ kfour $ boxToFewerBox 5

    describe "K4.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT kfour topM
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
        it (show f2)            $ not $ isProvableT kfour f2
        it "boxesBot 10"        $ not $ isProvableT kfour $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT kfour $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT kfour $ negBoxes 10        
        it "lobBoxes 10"        $ not $ isProvableT kfour $ lobBoxes 10
        it "boxToFewerBox 5"    $ not $ isProvableT kfour $ boxToFewerBox 5
  
  
    describe "S4.isProvableZ" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableZ sfour topM
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
        it "Top"                                                $ isProvableT sfour topM
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
        it "Top"                                                $ isProvableZ gl topM
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
        it (show f2)            $ not $ isProvableZ gl f2
        it "boxesBot 10"        $ not $ isProvableZ gl $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableZ gl $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableZ gl $ negBoxes 10        
        it "boxToFewerBox 5"    $ not $ isProvableZ gl $ boxToFewerBox 5

    describe "GL.isProvableT" $ do
      describe "Propositional formulas" $ do
        it "Top"                                                $ isProvableT gl topM
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
        it (show f2)            $ not $ isProvableT gl f2
        it "boxesBot 10"        $ not $ isProvableT gl $ boxesBot 10
        it "extraAtK 10"        $ not $ isProvableT gl $ extraAtK 10
        it "negBoxes 10"        $ not $ isProvableT gl $ negBoxes 10        
        it "boxToFewerBox 5"    $ not $ isProvableT gl $ boxToFewerBox 5

  -- Set a time limit.
  -- Test cases will be discarded if they take more than 5 seconds.
  let limit = 5 * 1000000 -- in microseconds

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

    -- to ensure laziness
    let implies x = if x then id else const True

    describe "If f and g isProvable, then Con f g isProvable" $ do
        prop "GenZ for CPL" $
          \ f g -> discardAfter limit $ (isProvableZ classical f && isProvableZ classical g) `implies` isProvableZ classical (ConP f g)
        prop "GenT for CPL" $
          \ f g -> discardAfter limit $ (isProvableT classical f && isProvableT classical g) `implies` isProvableT classical (ConP f g)
        prop "GenZ for IPL" $
          \ f g -> discardAfter limit $ (isProvableZ intui f && isProvableZ intui g) `implies` isProvableZ intui (ConP f g)
        prop "GenT for IPL" $
          \ f g -> discardAfter limit $ (isProvableT intui f && isProvableT intui g) `implies` isProvableT intui (ConP f g)
        prop "GenZ for K" $
          \ f g -> discardAfter limit $ (isProvableZ k f && isProvableZ k g) `implies` isProvableZ k (ConM f g)
        prop "GenT for K" $
          \ f g -> discardAfter limit $ (isProvableT k f && isProvableT k g) `implies` isProvableT k (ConM f g)
        prop "GenZ for K4" $
          \ f g -> discardAfter limit $ (isProvableZ kfour f && isProvableZ kfour g) `implies` isProvableZ kfour (ConM f g)
        prop "GenT for K4" $
          \ f g -> discardAfter limit $ (isProvableT kfour f && isProvableT kfour g) `implies` isProvableT kfour (ConM f g)
        prop "GenZ for S4" $
          \ f g -> discardAfter limit $ (isProvableZ sfour f && isProvableZ sfour g) `implies` isProvableZ sfour (ConM f g)
        prop "GenT for S4" $
          \ f g -> discardAfter limit $ (isProvableT sfour f && isProvableT sfour g) `implies` isProvableT sfour (ConM f g)
        prop "GenZ for GL" $
          \ f g -> discardAfter limit $ (isProvableZ gl f && isProvableZ gl g) `implies` isProvableZ gl (ConM f g)
        prop "GenT for GL" $
          \ f g -> discardAfter limit $ (isProvableT gl f && isProvableT gl g) `implies` isProvableT gl (ConM f g)


    describe "If f isProvable in CPL, then neg neg f isProvable in IPL" $ do
        prop "GenZ" $
          \ f -> discardAfter limit $ isProvableZ classical f `implies` isProvableZ intui (negP (negP f))
        prop "GenT" $
          \ f -> discardAfter limit $ isProvableT classical f `implies` isProvableT intui (negP (negP f))

    describe "Propositional tautologies in modal logics (GenZ)" $ modifyMaxSuccess (const 1000) $ do
      prop "K" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ k (pTom f)
      prop "K4" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ kfour (pTom f)
      prop "S4" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ sfour (pTom f)
      prop "GL" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ gl (pTom f)

    describe "Propositional tautologies in modal logics (GenT)" $ modifyMaxSuccess (const 1000) $ do
      prop "K" $
        \ f -> discardAfter limit $ isProvableT classical f === isProvableT k (pTom f)
      prop "K4" $
        \ f -> discardAfter limit $ isProvableT classical f === isProvableT kfour (pTom f)
      prop "S4" $
        \ f -> discardAfter limit $ isProvableT classical f === isProvableT sfour (pTom f)
      prop "GL" $
        \ f -> discardAfter limit $ isProvableT classical f === isProvableT gl (pTom f)

    describe "What is provable in K is also provable in K4" $ do
      prop "GenZ" $
        \ f -> discardAfter limit $ isProvableZ k f `implies` isProvableZ kfour f
      prop "GenT" $
        \ f -> discardAfter limit $ isProvableT k f `implies` isProvableT kfour f

    describe "What is provable in K4 is also provable in S4" $ do
      prop "GenZ" $
        \ f -> discardAfter limit $ isProvableZ kfour f `implies` isProvableZ sfour f
      prop "GenT" $
        \ f -> discardAfter limit $ isProvableT kfour f `implies` isProvableT sfour f

    describe "What is provable in K4 is also provable in GL" $ do
      prop "GenZ" $
        \ f -> discardAfter limit $ isProvableZ kfour f `implies` isProvableZ gl f
      prop "GenT" $
        \ f -> discardAfter limit $ isProvableT kfour f `implies` isProvableT gl f

    describe "f is provable in IPL iff its translation is provable in S4" $ do
      prop "GenZ" $
        \ f -> discardAfter limit $ isProvableZ intui f === isProvableZ sfour (translation f)
      prop "GenT" $
        \ f -> discardAfter limit $ isProvableT intui f === isProvableT sfour (translation f)
