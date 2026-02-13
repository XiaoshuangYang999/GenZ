module Logic.Modal.GL (gl) where

import qualified Data.Set as Set
import Logic.Modal.K
import General
import FormM

gl :: Logic FormM
gl = Log { name = "GL"
         , safeRules   = [leftBot, isAxiom, replaceRule safeML, isCycle]
         , unsafeRules = [box4rule]
         }

{-
CPL(safe) + isCycle(safe) + ☐4 rule(unsafe + without loopcheck):
       □Γ, Γ ⇒ φ
☐4    □Γ, Γ' ⇒ ∆, □φ
-}

isCycle :: Rule FormM
isCycle h fs _ = [("cycle", []) | fs `elem` h]

-- | The 4 box rule: without global loopcheck
box4rule :: Rule FormM
box4rule _ fs (Right (Box f)) = concatMap func ss where
  func :: Sequent FormM -> [(RuleName,[Sequent FormM])]
  func seqs = [("☐4", [seqs])]
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
box4rule _ _ _ = []
