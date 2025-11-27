module D4 where

import General
import K
import D
import K4
import FormM

dfour :: Logic FormM
dfour = Log { safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [fourrule,drule]}
-- Global and local loopchecks needed