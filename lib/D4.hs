module D4 where

import General
import K
import D
import K4
import FormM

dfour :: Logic FormM
dfour = Log { name = "D4"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [fourrule,drule]}
-- Global and local loopchecks needed