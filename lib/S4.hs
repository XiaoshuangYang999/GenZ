module S4 (sfour) where

import General
import K
import K4
import T

sfour :: Logic FormM
sfour = Log { safeRules   = [leftBotM, isAxiomM, additionRule safeML, trule]
            , unsafeRules = [fourrule] }