module Logic.Modal.K4 where

import qualified Data.Set as Set
import General
import Logic.Modal.K
import FormM

kfour :: Logic FormM
kfour = Log { name = "K4"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [box4rule]
            }

{-
CPL(safe) + ☐4 rule(unsafe + global loopcheck):
      Γ, □Γ ⇒ φ
☐4   Γ', □Γ ⇒ □φ, ∆
-}

box4rule :: Rule FormM
box4rule hs fs (Right (Box f)) = concatMap (globalLoopCheckMap "☐4" (fs:hs)) ss where
  -- add fs as new seqs could be a subset of fs
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
box4rule _ _ _ = []

-- Global loopcheck: if not already occur (as a subset) in the history.
globalLoopCheckMap :: RuleName -> History FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
globalLoopCheckMap r h seqs = [(r, [seqs]) | not $ any (seqs `Set.isSubsetOf`) h]
