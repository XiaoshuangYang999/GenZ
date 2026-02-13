module Logic.Modal.K where

import qualified Data.Set as Set
import Basics
import General
import FormM

k :: Logic FormM
k = Log { name = "K"
        , safeRules   = [leftBot, isAxiom, replaceRule safeML]
        , unsafeRules = [boxKrule]
        }

{-
CPL(safe) + ☐k rule(unsafe):
              Γ ⇒ φ
☐k       Γ', □Γ ⇒ □φ, ∆
-}

safeML :: Either FormM FormM -> [(RuleName,[Sequent FormM])]
safeML (Left (ConM f g))  = [("∧L", [Set.fromList [Left f, Left g]])]
safeML (Left (DisM f g))  = [("vL", map Set.singleton [Left f, Left g])]
safeML (Left (ImpM f g))  = [("→L", map Set.singleton [Right f, Left g])]
safeML (Right (ConM f g)) = [("∧R", map Set.singleton [Right f, Right g])]
safeML (Right (DisM f g)) = [("vR", [Set.fromList [Right g, Right f]])]
safeML (Right (ImpM f g)) = [("→R", [Set.fromList [Right g, Left f]])]
safeML _                  = []

boxKrule :: Rule FormM
boxKrule _ fs (Right (Box f)) = Set.toList $ Set.map (func f) $ Set.powerSet . removeBoxLeft $ fs where
  func :: FormM -> Sequent FormM -> (RuleName,[Sequent FormM])
  func g seqs = ("☐k", [Set.insert (Right g) seqs])
boxKrule _ _ _ = []

removeBoxLeft :: Sequent FormM -> Sequent FormM
removeBoxLeft  = setComprehension isLeftBox fromBox

isLeftBox :: Either FormM FormM -> Bool
isLeftBox (Left (Box _)) = True
isLeftBox _              = False

fromBox :: Either FormM FormM -> Either FormM FormM
fromBox (Left  (Box g)) = Left g
fromBox (Right (Box g)) = Right g
fromBox g = g
