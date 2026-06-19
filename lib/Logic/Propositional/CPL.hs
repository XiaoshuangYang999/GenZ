module Logic.Propositional.CPL (classical) where

import qualified Data.Set as Set
import General
import FormP

classical :: Logic FormP
classical = Log { name = "CPL"
                , safeRules   = [leftBot, isAxiom, replaceRule safeCPL]
                , unsafeRules = []
                }

{-
   Γ, φ, ψ  ⇒ ∆
∧L Γ, φ ∧ ψ ⇒ ∆
   Γ, φ ⇒ ∆    Γ, ψ ⇒ ∆
∨L Γ, φ ∨ ψ ⇒ ∆
   Γ ⇒ ∆, φ    Γ ⇒ ∆, ψ
→L Γ, φ → ψ ⇒ ∆
   Γ ⇒ ∆, φ    Γ ⇒ ∆, ψ
∧R Γ ⇒ ∆, φ ∧ ψ
   Γ ⇒ ∆, φ, ψ
∨R Γ ⇒ ∆, φ ∨ ψ
   Γ, φ ⇒ ∆, ψ
→R Γ ⇒ ∆, φ → ψ
-}

safeCPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeCPL (Left (ConP f g))   = [("∧L", [Set.fromList [Left g, Left f]])]
safeCPL (Left (DisP f g))   = [("vL", [Set.singleton (Left f), Set.singleton (Left g)])]
safeCPL (Left (ImpP f g))   = [("→L", [Set.singleton (Right f), Set.singleton (Left g)])]
safeCPL (Right (ConP f g))  = [("∧R", [Set.singleton (Right f), Set.singleton (Right g)])]
safeCPL (Right (DisP f g))  = [("vR", [Set.fromList [Right g, Right f]])]
safeCPL (Right (ImpP f g))  = [("→R", [Set.fromList [Right g, Left f]])]
safeCPL _                   = []
