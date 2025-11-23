module D4 where

import General
import K
import D
import K4

dfour :: Logic FormM
dfour = Log { safeRules   = [leftBotM, isAxiomM, replaceRule safeML]
            , unsafeRules = [fourrule,drule]}