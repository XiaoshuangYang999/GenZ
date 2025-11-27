module CPL (classical) where

import qualified Data.Set as Set

import General
import FormP

classical :: Logic FormP
classical = Log { safeRules   = [leftBot, isAxiom, replaceRule safeCPL]
                , unsafeRules = [] }

safeCPL :: Either FormP FormP -> [(RuleName,[Sequent FormP])]
safeCPL (Left (ConP f g))   = [("∧L", [Set.fromList [Left g, Left f]])]
safeCPL (Left (DisP f g))   = [("vL", map Set.singleton [Left f, Left g])]
safeCPL (Left (ImpP f g))   = [("→L", map Set.singleton [Left g, Right f])]
safeCPL (Right (ConP f g))  = [("∧R", map Set.singleton [Right f, Right g])]
safeCPL (Right (DisP f g))  = [("vR", [Set.fromList [Right g, Right f]])]
safeCPL (Right (ImpP f g))  = [("→R", [Set.fromList [Right g, Left f]])]
safeCPL _                   = []
