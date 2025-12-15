module Logic.Modal.D45 where

import qualified Data.Set as Set
import General
import Logic.Modal.K
import Logic.Modal.K4
import Logic.Modal.K45
import FormM

dfourfive :: Logic FormM
dfourfive = Log { name = "D45"
            , safeRules   = [leftBot, isAxiom, replaceRule safeML]
            , unsafeRules = [kfourfiverule,dfourfiverule] }

{-
CPL + k45 rule(global loopcheck) + d45 rule(global loopcheck):
          □Γ1, Γ2 ⇒ □∆, φ
☐k45  Γ', □Γ1, □Γ2⇒ □∆, □φ, ∆'        ∆ must be nonempty
          □Γ1, Γ2 ⇒ □∆
☐d45  Γ', □Γ1, □Γ2⇒ □∆, ∆'            ∆ must be nonempty

-}

dfourfiverule :: Rule FormM
dfourfiverule hs fs _ =
  concatMap (globalLoopCheckMap "☐d45" (fs:hs)) premises
 where
  -- { □Γ1 ∪ □Γ2 }
  lBoxes = Set.filter isLeftBox fs
  -- { □Δ }
  rBoxes = Set.filter isRightBox fs
  -- { Δ }
  deltaS :: [Set.Set (Either FormM FormM)]
  deltaS = Set.toList (Set.powerSet rBoxes)
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
        ]
    | delta <- deltaS
    , (boxGamma1, boxGamma2) <- boxGammaPartitions
    ]