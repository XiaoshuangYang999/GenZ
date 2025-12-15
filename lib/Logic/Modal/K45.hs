module Logic.Modal.K45 where

import qualified Data.Set as Set
import General
import Logic.Modal.K
import Logic.Modal.K4
import FormM

kfourfive :: Logic FormM
kfourfive = Log { name = "K45"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [kfourfiverule] }

{-
CPL + k45 rule(global loopcheck):
          □Γ1, Γ2 ⇒ □∆, φ
☐k45  Γ', □Γ1, □Γ2⇒ □∆, □φ, ∆'         ∆ must be nonempty
-}

kfourfiverule :: Rule FormM
kfourfiverule hs fs (Right (Box f)) =
  concatMap (globalLoopCheckMap "☐k45" (fs:hs)) premises
 where
  -- { □Γ1 ∪ □Γ2 }
  lBoxes = Set.filter isLeftBox fs
  -- { □Δ }
  rBoxesRemove = Set.delete (Right (Box f)) (Set.filter isRightBox fs)
  -- { Δ }
  deltaS :: [Set.Set (Either FormM FormM)]
  deltaS = Set.toList (Set.powerSet rBoxesRemove)
  -- [(□Γ1, □Γ2)]
  boxGammaPartitions :: [(Set.Set (Either FormM FormM), Set.Set (Either FormM FormM))]
  boxGammaPartitions = partitionDrop lBoxes
  -- □Γ1, Γ2 ⇒ □Δ, φ
  premises :: [Set.Set (Either FormM FormM)]
  premises =
    [ Set.unions
        [ boxGamma1
        , Set.map fromBox boxGamma2
        , delta
        , Set.singleton (Right f)
        ]
    | delta <- deltaS
    , (boxGamma1, boxGamma2) <- boxGammaPartitions
    ]
kfourfiverule _ _ _ = []

-- Generate all ordered partitions of a set. O(n·2^n)
partitionDrop :: Ord a => Set.Set a -> [(Set.Set a, Set.Set a)]
partitionDrop s =
  [ (Set.fromDistinctAscList ls, Set.fromDistinctAscList rs)
  | (ls, rs) <- go (Set.toAscList s)
  ]
  where
    -- go produces (leftElemsAsc, rightElemsAsc)
    go []     = [([], [])]
    go (x:xs) =
      let rest = go xs
      in  [(l, r) | (l, r) <- rest]   -- drop x
       ++ [(l, x:r) | (l, r) <- rest] -- x ∈ Γ1
       ++ [(x:l, r) | (l, r) <- rest] -- x ∈ Γ2

isRightBox :: Either FormM FormM -> Bool
isRightBox (Right (Box _)) = True
isRightBox _              = False