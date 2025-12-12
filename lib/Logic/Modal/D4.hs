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

{-
CPL + d rule + 4 rule(global loopcheck):
          Γ, φ ⇒
☐d  Γ', □Γ, □φ ⇒ ∆
         Γ, □Γ ⇒ φ
☐4      Γ', □Γ ⇒ □φ, ∆
-}