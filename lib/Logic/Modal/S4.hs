module Logic.Modal.S4 (sfour) where

import General
import Logic.Modal.K
import Logic.Modal.K4
import Logic.Modal.T
import FormM

sfour :: Logic FormM
sfour = Log { name = "S4"
            , safeRules   = [leftBot, isAxiom, additionRule safeML, trule]
            , unsafeRules = [fourrule] }
