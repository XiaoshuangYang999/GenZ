module D where

import qualified Data.Set as Set
import General
import K
import FormM

d :: Logic FormM
d = Log { name = "D"
        , safeRules   = [leftBot, isAxiom, replaceRule safeML]
        , unsafeRules = [krule,drule]}

-- | The D rule.
drule :: Rule FormM
drule _ fs (Left (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet . removeBoxLeft $ Set.delete (Left (Box f)) fs where
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("☐d", [Set.insert (Left g) seqs])
drule _ _ _ = []

-- no loopcheck is needed? global?