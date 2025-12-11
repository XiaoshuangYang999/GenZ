module Logic.Modal.D4 where

import General
import Logic.Modal.K
import Logic.Modal.D
import Logic.Modal.K4
import FormM

dfour :: Logic FormM
dfour = Log { name = "D4"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [fourrule,drule]}
-- Global loopcheck needed for 4 rule
