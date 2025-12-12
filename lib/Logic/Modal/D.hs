module Logic.Modal.D where

import qualified Data.Set as Set
import General
import Logic.Modal.K
import FormM

d :: Logic FormM
d = Log { name = "D"
        , safeRules   = [leftBot, isAxiom, replaceRule safeML]
        , unsafeRules = [krule,drule]}

{-
CPL + k rule + d rule:
              Γ ⇒ φ
☐k       Γ', □Γ ⇒ □φ, ∆
           Γ, φ ⇒
☐d   Γ', □Γ, □φ ⇒ ∆
-}

drule :: Rule FormM
drule _ fs (Left (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet . removeBoxLeft $ Set.delete (Left (Box f)) fs where
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("☐d", [Set.insert (Left g) seqs])
drule _ _ _ = []
