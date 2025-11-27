module S4 (sfour) where

import General
import K
import K4
import T
import FormM

sfour :: Logic FormM
sfour = Log { safeRules   = [leftBot, isAxiom, additionRule safeML, trule]
            , unsafeRules = [fourrule] }