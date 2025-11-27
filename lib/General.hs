{-# LANGUAGE InstanceSigs, DeriveGeneric, FlexibleInstances, LambdaCase #-}

module General where

import Control.Monad
import Data.GraphViz
import Data.GraphViz.Types.Monadic hiding ((-->))
import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import System.IO (hGetContents)
import System.Process
import Basics

type Sequent f = Set (Either f f)

type RuleName = String

data Proof f = Node (Sequent f) (Maybe (RuleName, [Proof f])) Bool
  deriving (Eq,Ord,Show)
-- always starts with false until turned true

-- Extract truth from a proof
getTruth :: Proof f -> Bool
getTruth (Node _ _ b) = b

-- Manipulate the parent truth
allTrue :: [Proof f] -> Bool
allTrue = all getTruth

-- * Histories, Rules, Logics

type History f = [Sequent f]

class HasHistory a where
  histOf :: a f -> History f

class HasProof a where
  proofOf :: a f -> Proof f

-- | A `Rule` takes the history/branch, current sequent and an active formula.
-- It returns ways to apply a rule, each resulting in possibly multiple branches.
type Rule f = History f -> Sequent f -> Either f f -> [(RuleName, [Sequent f])]

-- | A replace rule only takes an active formula.
replaceRule :: (Eq f, Ord f) => (Either f f -> [(RuleName, [Sequent f])]) -> Rule f
replaceRule fun _ fs g =
  [ ( fst . head $ fun g
    , [ Set.delete g fs `Set.union` newfs
      | newfs <- snd . head $ fun g ]
    )
  | not (null (fun g)) ]

isApplicable :: History f -> Sequent f -> Either f f -> Rule f -> Bool
isApplicable hs fs f r = not . null $ r hs fs f

isApplicableRule :: History f -> Sequent f -> Rule f -> Bool
isApplicableRule hs fs r = any (\f -> isApplicable hs fs f r) fs

-- | A Logic for a formula type `f`.
data Logic f = Log { name :: String
                   , safeRules   :: [Rule f]
                   , unsafeRules :: [Rule f] }

-- | A prover takes a logic and a formula and returns a Boolean.
type Prover f = Logic f -> f -> Bool

-- * Tree Proofs

newtype ProofWithH f = HP (History f, Proof f)

instance HasHistory ProofWithH where
  histOf (HP (hs, _)) = hs

instance HasProof ProofWithH where
  proofOf (HP (_, pf)) = pf

-- * Zip Proofs

data ZipProof f = ZP (Proof f) (ZipPath f)
-- only switch path when this branch is closed
-- each sub-proof tree should remember whether it's closed

data ZipPath f = Top | Step (Sequent f) RuleName (ZipPath f) [Proof f] [Proof f]

instance HasHistory ZipPath where
  histOf :: ZipPath f -> History f
  histOf Top = []
  histOf (Step xs _ p _ _) = xs : histOf p

instance HasHistory ZipProof where
  histOf :: ZipProof f -> History f
  histOf (ZP _ zpath) = histOf zpath

instance HasProof ZipProof where
  proofOf (ZP pf _) = pf
  -- proofOf (ZP pf Top) = pf  -- extract the proof at top
  -- proofOf _ = error "Did not go back to Top"
-- to check for errors

-- * Tree-based prover

startForT :: f -> ProofWithH f
startForT f =  HP ([], Node (Set.singleton (Right f)) Nothing False)

extendT  :: (Eq f, Show f, Ord f) => Logic f -> ProofWithH f -> [ProofWithH f]
extendT l pt@(HP (h, Node fs Nothing _)) =
  case ( List.filter (isApplicableRule h fs) (safeRules l)
       , unsafeRules l ) of
  -- The safe rule r can be applied:
  (r:_ , _       ) ->
    [ HP (h, Node fs (Just (therule, map proofOf ts)) $ allTrue $ map proofOf ts)
    | (therule, result) <- r h fs f
    , ts <- pickOneOfEach [ extendT l (HP (fs : h, Node newSeqs Nothing False))
                          | newSeqs <- result ] ]
    where f = Set.elemAt 0 $ Set.filter (\g -> isApplicable h fs g r) fs
              -- (Using the first possible principal formula.)
  -- At least one unsafe rule can be applied:
  ([], rs@(_:_)) -> List.concatMap applyRule rs
    where
      applyRule r = case List.filter (getTruth . proofOf) nps of
                    tp : _ -> [tp]
                    [] -> [HP (h, Node fs Nothing False)]
        where
          gs = Set.filter (\g -> isApplicable h fs g r) fs
          nps = concat $ List.concatMap tryExtendT gs
          tryExtendT g = [ List.map (\pwh -> HP (h, Node fs (Just (therule, [proofOf pwh])) $ allTrue [proofOf pwh]))
                           $ extendT l (HP (fs : h, Node (head result) Nothing False))
                           -- (Using head because we never have branching unsafeRules.)
                         | (therule, result) <- r (histOf pt) fs g ]
  -- No rule can be applied, leave proof unfinished:
  ([], []      ) -> [HP (h, Node fs Nothing False)]
extendT _ (HP (_,Node _ (Just _) _)) = error "already extended"

proveT :: (Eq f, Show f,Ord f) => Logic f -> f -> [Proof f]
proveT l f = List.map proofOf $ extendT l (startForT f)

isProvableT :: (Eq f, Show f, Ord f) => Prover f
isProvableT l f = any getTruth (proveT l f)

proveprintT :: (Eq f, Show f,Ord f) => Logic f -> f -> Proof f
proveprintT l f = case List.filter getTruth (proveT l f) of
                    pf:_ -> pf
                    []   -> head (proveT l f)

provePdfT :: (Ord f,Show f, Eq f) => Logic f -> f -> IO FilePath
provePdfT l f= pdf $ proveprintT l f

-- * Zipper-based prover

instance TreeLike ZipProof where
  zsingleton x                               = ZP (Node (Set.singleton (Right x)) Nothing False) Top
  move_left (ZP c (Step s r p (x:xs) ys))    = ZP x (Step s r p xs (c:ys))
  move_left _                                = error "cannot go left"
  -- no need to change the Bool
  move_right (ZP c (Step s r p xs (y:ys)))   = ZP y (Step s r p (c:xs) ys)
  move_right _                               = error "cannot go right"
  -- no need to change the Bool
  move_up (ZP c@(Node _ _ t) (Step s r p xs ys)) = ZP (Node s (Just (r, c:xs ++ ys)) $ t && allTrue (xs ++ ys)) p
  move_up _                                  = error "cannot go up"
  -- dangerous! update parent's truth
  move_down (ZP (Node s (Just (r, x:xs)) _) p) = ZP x (Step s r p [] xs)
  move_down _                                = error "cannot go down"
  zdelete (ZP _ (Step s _ Top _ _))          = ZP (Node s Nothing False) Top
  zdelete (ZP _ (Step s _ p _ _))            = ZP (Node s Nothing False) p
  zdelete _                                  = error "cannot delete top"
    -- parent true iff both children true
    -- parent's truth condition does not affect the child
    -- closed proof/ true -> ax or Lbot -> Just ("",[])
    -- for genT, then parent's truth condition should be decided by eventually checking the truth condition of children
    -- for genZ, truth is manipulated eventually by closing branches and switching branches, eventually going to the top
    -- whenever moving to a higher node (a parent node), change the truth condition by checking the children.
    -- Done by move_up!

-- Was it used?
fromZip :: ZipProof f -> Proof f
fromZip (ZP x Top) = x
fromZip zp = fromZip (move_up zp)

-- | Does the node have a right sibling?
hasRsibi :: ZipPath f -> Bool
hasRsibi (Step _ _ _ _ (_:_))= True
hasRsibi _ = False

-- | Does the node have a left sibling?
hasLsibi :: ZipPath f -> Bool
hasLsibi (Step _ _ _ (_:_) _ )= True
hasLsibi _ = False

-- | Switch path, left-biased
-- all the truth manipulations are done by TreeLike method
switch :: ZipProof f -> ZipProof f
switch (ZP pf Top) = ZP pf Top
switch (ZP pf p) = if hasRsibi p
                      then move_right (ZP pf p)
                      else switch.move_up $ ZP pf p

reverseSwitch :: ZipProof f -> ZipProof f
reverseSwitch (ZP pf Top) = ZP pf Top
reverseSwitch (ZP pf p) = if hasLsibi p
                      then move_left (ZP pf p)
                      else reverseSwitch.move_up $ ZP pf p

leftAllClosed :: ZipProof f -> Bool
leftAllClosed (ZP _ Top) = True
leftAllClosed zp = getTruth (proofOf zp) && leftAllClosed (reverseSwitch zp)


startForZ :: f -> ZipProof f
startForZ f = ZP (Node (Set.singleton (Right f)) Nothing False) Top

extendZ  :: (Ord f,Eq f) => Logic f -> ZipProof f -> [ZipProof f]
extendZ l zp@(ZP (Node fs Nothing _) p) =
  case ( List.filter (isApplicableRule (histOf zp) fs) (safeRules l)
       , unsafeRules l) of
  -- The safe rule r can be applied:
  (r:_ , _       )    ->  let f = Set.elemAt 0 $ Set.filter (\g -> isApplicable (histOf zp) fs g r) fs
                              (therule,results) = head $ r (histOf zp) fs f
                              newPf             = Node fs (Just (therule, [Node newSeq Nothing False | newSeq <- results]))
                              -- The truth condition of the parent will always be false, until set true during the switch
                              -- If results null, True here of course. We will switch branch
                              nextZP
                                | null results  = switch    $ ZP (newPf True) p-- no children, i.e. proved
                                | otherwise     = move_down $ ZP (newPf False) p -- keep extending the child node
                          in extendZ l nextZP
  -- At least one unsafe rule can be applied:
  ([], rs@(_:_))    -> List.concatMap applyRule rs
    where
      applyRule r = case List.filter leftAllClosed nps of
                    np : _ -> [np]                           -- the closed proof
                    []     -> [ZP (Node fs Nothing False) p]
        where
          gs = Set.filter (\g -> isApplicable (histOf zp) fs g r) fs
          nps = concat $ List.concatMap tryExtendZ gs
          tryExtendZ g = [ extendZ l (ZP (Node (head result) Nothing False) (Step fs therule p [] []) )
                         -- head result as unsaferules won't branch
                         | (therule,result) <- r (histOf zp) fs g ]
  -- No rule can be applied, leave proof unfinished:
  ([], []      )    -> [ZP (Node fs Nothing False) p] -- we won't return to top in this case
  -- if provable, will return to top. Not otherwise

extendZ _ zp@(ZP (Node _ (Just _ ) _) _) = [zp] -- needed after switch

-- The easiest way to check whether a zipproof is closed is by always going back to Top and manipulate the truth on the way
-- Then to check the truth of the whole proof, we only need to check the truth condition at the top

-- Return a list of proofs
-- getProof should do it as we should return to Top at the end
proveZ :: (Eq f, Ord f) => Logic f -> f -> [Proof f]
proveZ l f = List.map fromZip $ proveZZ l f

proveZZ :: (Eq f, Ord f) => Logic f -> f -> [ZipProof f]
proveZZ l f = extendZ l (startForZ f)

isProvableZ :: (Eq f, Ord f) => Prover f
isProvableZ l f = any (getTruth . proofOf) $ List.filter isTop $ proveZZ l f

isTop :: ZipProof f -> Bool
isTop (ZP _ Top) = True
isTop _ = False

proveprintZ :: (Eq f, Ord f) => Logic f -> f -> Proof f
proveprintZ l f = case List.filter (\zp -> getTruth (proofOf zp) && isTop zp) $ proveZZ l f of
                    zf:_ -> proofOf zf
                    []   -> fromZip . head $ proveZZ l f
-- perhaps improve it later

provePdfZ :: (Show f, Eq f,Ord f) => Logic f -> f -> IO FilePath
provePdfZ l f = pdf $ proveprintZ l f

-- * GraphViz and LaTeX output

-- Pretty printing a list of f's
ppList :: Show f => [f] -> String
ppList = intercalate " , " . map show

-- Pretty printing a set of f's
ppForm :: Show f => Set f -> String
ppForm ms = ppList (Set.toList ms)

-- Pretty printing a sequent of f;s
ppSeq :: (Show f, Ord f) => Sequent f -> String
ppSeq xs = ppForm (leftsSet xs) ++ " => " ++ ppForm (rightsSet xs)

-- Now the DispAble will not show Bool. But can be modified
instance (Show f,Ord f) => (DispAble (Proof f)) where
  toGraph = toGraph' "" where
    toGraph' pref (Node fs Nothing _) = do
      node pref [shape PlainText, toLabel $ ppSeq fs]
      node (pref ++ "open") [shape PlainText, toLabel "?"]
      edge pref (pref ++ "open") []
    toGraph' pref (Node fs (Just (rule',ts)) _) = do
      node pref [shape PlainText, toLabel $ ppSeq fs]
      if null ts then do
        node pref [shape PlainText, toLabel $ ppSeq fs]
        node (pref ++ "closed") [shape PlainText, toLabel "."]
        edge pref (pref ++ "closed") [toLabel rule']
      else mapM_ (\(t,y') -> do
        toGraph' (pref ++ show y' ++ ":") t
        edge pref (pref ++ show y' ++ ":") [toLabel rule']
        ) (zip ts [(0::Integer)..])

class TeX a where
  tex :: a -> String
  texFile :: a -> IO ()
  texFile x = do
    let
      pre = unlines [ "\\documentclass[border=2pt]{standalone}"
                   , "\\usepackage[utf8]{inputenc}"
                   , "\\usepackage{bussproofs,fontenc,graphicx,amssymb,amsmath}"
                   , "\\usepackage[pdftex]{hyperref}"
                   , "\\hypersetup{pdfborder={0 0 0},breaklinks=true}"
                   , "\\begin{document}" ]
      post = "\\DisplayProof\n\\end{document}"
    writeFile "temp.tex" (pre ++ tex x ++ post)
    (_inp, _out, err, pid) <- runInteractiveCommand "pdflatex -interaction=nonstopmode temp.tex"
    _ <- waitForProcess pid
    hGetContents err >>= (\e -> unless (null e) (putStrLn e))

instance (Ord f, TeX f) => TeX (Sequent f) where
  tex xs = texList (Set.toList $ leftsSet xs) ++ " \\Rightarrow " ++ texList (Set.toList $ rightsSet xs)

texList :: TeX f => [f] -> String
texList = intercalate " , " . map (removeOutsideBrackets . tex) where
  removeOutsideBrackets ('(':rest) = init rest
  removeOutsideBrackets s = s

texRuleName :: RuleName -> String
texRuleName r = "$" ++ concatMap f r ++ "$" where
  f = \case
    'v' -> "\\lor"
    '→' -> "\\to"
    '∧' -> "\\land"
    'R' -> "_{\\mathsf{R}}"
    'L' -> "_{\\mathsf{L}}"
    'i' -> "^{i}"
    'T' -> "\\mathsf{T}"
    'a' -> "\\mathsf{a}"
    'x' -> "\\mathsf{x}"
    'c' -> "\\mathsf{c}"
    'y' -> "\\mathsf{y}"
    'l' -> "\\mathsf{l}"
    'e' -> "\\mathsf{e}"
    '⊥' -> "\\bot"
    '4' -> "_{\\mathsf{4}}"
    'k' -> "_{\\mathsf{k}}"
    '☐' -> "\\Box "
    c -> [c]

-- | Generate LaTeX code to show a proof using the buss package.
-- Does not include @\DisplayProof@ yet.
toBuss :: (Show f, TeX f, Ord f) => Proof f -> String
toBuss (Node fs Nothing _) = "\\AxiomC{ $ " ++ tex fs ++ " $ }"
toBuss (Node fs (Just (rule', ts)) _) =
  concatMap toBuss ts
  ++
  case length ts of
  0 -> "\\AxiomC{\\phantom{I}}\n " ++ r ++ "\\UnaryInfC{ $" ++ tex fs ++ "$ }\n"
  1 -> r ++ "\\UnaryInfC{ $" ++ tex fs ++  "$ }\n"
  2 -> r ++ "\\BinaryInfC{ $" ++ tex fs ++  "$ }\n"
  _ -> error "too many premises"
  where r = "\\LeftLabel{" ++ texRuleName rule' ++ "}\n"

instance (Show f, TeX f, Ord f) => TeX (Proof f) where
  tex = toBuss

-- * The Language
type Atom = String

-- | This formula type contains propositional logic.
class PropLog f where
  neg :: f -> f
  con :: f -> f ->f
  dis :: f -> f ->f
  top :: f
  iff :: f -> f -> f
  isAtom :: f -> Bool
  isAxiom :: Rule f
  leftBot :: Rule f

swap :: Either a b -> Either b a
swap (Left x) = Right x
swap (Right x) = Left x