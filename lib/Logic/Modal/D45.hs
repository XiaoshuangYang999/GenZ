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
                , unsafeRules = [boxK45rule,boxD45rule]
                }

{-
CPL(safe) + ☐k45 rule(unsafe + global loopcheck) + ☐d45 rule(unsafe + global loopcheck):
          □Γ1, Γ2 ⇒ □∆, φ
☐k45  Γ', □Γ1, □Γ2⇒ □∆, □φ, ∆'
          □Γ1, Γ2 ⇒ □∆
☐d45  Γ', □Γ1, □Γ2⇒ □∆, ∆'
-}

boxD45rule :: Rule FormM
boxD45rule hs fs _ =
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
