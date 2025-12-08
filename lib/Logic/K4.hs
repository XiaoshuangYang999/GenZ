module Logic.K4 where

import qualified Data.Set as Set

import General
import Logic.K
import FormM

kfour :: Logic FormM
kfour = Log { name = "K4"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [fourrule] }

-- | The 4 box rule.
fourrule :: Rule FormM
fourrule hs fs (Right (Box f)) = concatMap (globalLoopCheckMap (fs:hs)) ss where
  -- add fs as new seqs could be a subset of fs
  ss = Set.map (\s -> Set.unions [Set.singleton (Right f), s, Set.map fromBox s]) ss'
  ss' = Set.powerSet $ Set.filter isLeftBox fs
fourrule _ _ _ = []

-- Global loopcheck: not already occur (as a subset) in the history.
globalLoopCheckMap :: History FormM -> Sequent FormM -> [(RuleName,[Sequent FormM])]
globalLoopCheckMap h seqs = [("☐4", [seqs]) | not $ any (seqs `Set.isSubsetOf`) h]
