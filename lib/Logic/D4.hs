module Logic.D4 where

import General
import Logic.K
import Logic.D
import Logic.K4
import FormM

dfour :: Logic FormM
dfour = Log { name = "D4"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [fourrule,drule]}
-- Global and local loopchecks needed
