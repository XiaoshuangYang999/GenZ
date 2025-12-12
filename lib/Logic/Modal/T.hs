module Logic.Modal.T where

import qualified Data.Set as Set
import Data.List as List
import General
import Logic.Modal.K
import FormM

t :: Logic FormM
t = Log { name = "T"
        , safeRules   = [leftBot, isAxiom, additionRule safeML,trule]
        , unsafeRules = [krule]}

{-
saturated CPL(local loopcheck) + k rule + t rule(safe +local loopcheck):
           Γ ⇒ φ
☐k    Γ', □Γ ⇒ □φ, ∆
    φ, □φ, Γ ⇒ ∆
☐t     □φ, Γ ⇒ ∆
-}

-- | The T box rule. Involve local loopcheck
trule :: Rule FormM
trule _ fs (Left (Box f)) = [("☐t", [Set.insert (Left f) fs]) | Left f `notElem` fs]
trule _ _ _ = []

-- | Local loopcheck: Is this sequent saturated?
localLoopCheck :: Sequent FormM -> Either FormM FormM -> Bool
localLoopCheck fs f = case safeML f of []               -> False
                                       ((_,results):_)  -> not $ any (`Set.isSubsetOf` fs) results

-- Similar as the one in IPL
additionRule :: (Either FormM FormM -> [(RuleName, [Sequent FormM])]) -> Rule FormM
additionRule fun _ fs g =
  [ ( fst . head $ fun g
    , [ fs `Set.union` newfs | newfs <- snd . head $ fun g ] -- not deleting `g` here!
    )
  | localLoopCheck fs g
  , not (List.null (fun g)) ]