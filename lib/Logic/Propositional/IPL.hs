module Logic.Propositional.IPL (intui) where

import Data.List as List
import qualified Data.Set as Set
import General
import Basics
import FormP

intui :: Logic FormP
intui = Log { name = "IPL"
            , safeRules   = [leftBot, isAxiom, additionRule safeIPL]
            , unsafeRules = [additionRuleNoLoop unsafeIPL]
            }

{-
-- Saturated saferules(local loopcheck)
    Γ, φ ∧ ψ, φ, ψ  ⇒ ∆
∧L  Γ, φ ∧ ψ ⇒ ∆
    Γ, φ ∨ ψ, φ ⇒ ∆    Γ, φ ∨ ψ, ψ ⇒ ∆
∨L  Γ, φ ∨ ψ ⇒ ∆
    Γ, φ → ψ ⇒ ∆, φ    Γ, φ → ψ, ψ ⇒ ∆
→iL Γ, φ → ψ ⇒ ∆
    Γ ⇒ ∆, φ ∧ ψ, φ    Γ ⇒ ∆, φ ∧ ψ, ψ
∧R  Γ ⇒ ∆, φ ∧ ψ
    Γ ⇒ ∆, φ ∨ ψ, φ, ψ
∨R  Γ ⇒ ∆, φ ∨ ψ
-- Unsaferule(local + global loopcheck)
    Γ, φ ⇒ ψ
→iR Γ ⇒ ∆, φ → ψ
-}

safeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeIPL (Left (ConP f g))  = [("∧L" , [Set.fromList [Left g, Left f]])]
safeIPL (Left (DisP f g))  = [("vL" , [Set.singleton (Left f), Set.singleton (Left g)])]
safeIPL (Left (ImpP f g))  = [("→iL", [Set.singleton (Right f), Set.singleton (Left g)])]
safeIPL (Right (ConP f g)) = [("∧R" , [Set.singleton (Right f), Set.singleton (Right g)])]
safeIPL (Right (DisP f g)) = [("vR" , [Set.fromList [Right g, Right f]])]
safeIPL _                  = []

-- | The R-> rule.
unsafeIPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
unsafeIPL (Right (ImpP f g)) = [("→iR", [Set.fromList [Right g, Left f]])]
unsafeIPL  _                 = []

-- | Local loopcheck: Is this sequent saturated?
localLoopCheck :: Sequent FormP -> Either FormP FormP -> Bool
localLoopCheck fs f@(Right (ImpP _ _)) = not $ any (`Set.isSubsetOf` fs) (snd . head . unsafeIPL $ f)
localLoopCheck fs f = case safeIPL f of []              -> False
                                        ((_,results):_) -> not $ any (`Set.isSubsetOf` fs) results

-- * IPL-specific versions of `replaceRule`.
-- | Like `replaceRule` but keep active formula (built-in weakening), and block when localLoopCheck.
additionRule :: (Either FormP FormP -> [(RuleName, [Sequent FormP])]) -> Rule FormP
additionRule fun _ fs g =
  [ ( fst . head $ fun g
    , [ fs `Set.union` newfs | newfs <- snd . head $ fun g ] -- not deleting `g` here!
    )
  | localLoopCheck fs g -- local loopcheck
  , not (List.null (fun g)) ]

-- | Helper function for replaceRuleIPLunsafe.
applyIPL :: Sequent FormP -> Either FormP FormP -> [Sequent FormP] -> [Sequent FormP]
applyIPL fs _ = List.map (leftOfSet fs `Set.union`)

-- | Like `additionRule` but also doing a global loopcheck.
additionRuleNoLoop :: (Either FormP FormP -> [(RuleName, [Sequent FormP])]) -> Rule FormP
additionRuleNoLoop fun h fs g =
  [ ( fst . head $ fun g
    , applyIPL fs g $ snd . head . fun $ g
    )
  | localLoopCheck fs g -- local loopcheck
  , globalLoopCheck h fs g -- gobal loopcheck
  , not (List.null (fun g)) ]

-- | Check that the result of applying `unsafeIPL` to `f` does
-- not already occur (as a subset) in the history.
-- Helper function for `replaceRuleIPLunsafe`.
globalLoopCheck :: [Sequent FormP] -> Sequent FormP -> Either FormP FormP -> Bool
globalLoopCheck hs fs f@(Right (ImpP _ _)) =
  let xs = applyIPL fs f (snd (head (unsafeIPL f)))
  in not $ any (Set.isSubsetOf (head xs)) hs
globalLoopCheck _ _ _ = False
