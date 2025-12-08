module Logic.S4 (sfour) where

import General
import Logic.K
import Logic.K4
import Logic.T
import FormM

sfour :: Logic FormM
sfour = Log { name = "S4"
            , safeRules   = [leftBot, isAxiom, additionRule safeML, trule]
            , unsafeRules = [fourrule] }
