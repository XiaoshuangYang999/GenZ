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

{-
saturated CPL(local loopcheck) + t rule(safe + local loopcheck) + 4 rule(global loopcheck):
    φ, □φ, Γ ⇒ ∆
☐t     □φ, Γ ⇒ ∆
       Γ, □Γ ⇒ φ
☐4    Γ', □Γ ⇒ □φ, ∆
-}