{-# LANGUAGE InstanceSigs, DeriveGeneric, FlexibleInstances, LambdaCase #-}

module General where

import Control.Monad
import Data.GraphViz
import Data.GraphViz.Types.Monadic hiding ((-->))
import Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import System.IO (hGetContents)
import System.Process
import Test.QuickCheck

import Basics

type Sequent f = Set (Either f f)

type RuleName = String

data Proof f = Node (Sequent f) (Maybe (RuleName, [Proof f]))
  deriving (Eq,Ord,Show)

isClosedPf :: Eq f => Proof f -> Bool
isClosedPf (Node _ Nothing) = False
isClosedPf (Node _ (Just (_,ts))) = all isClosedPf ts

-- * Histories, Rules, Logics

type History f = [Sequent f]

class HasHistory a where
  histOf :: a f -> History f

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
data Logic f = Log { safeRules   :: [Rule f]
                   , unsafeRules :: [Rule f] }

-- | A prover takes a logic and a formula and returns a Boolean.
type Prover f = Logic f -> f -> Bool

-- * Tree Proofs

newtype ProofWithH f = HP (History f, Proof f)

instance HasHistory ProofWithH where
  histOf (HP (hs, _)) = hs

hpSnd :: ProofWithH f -> Proof f
hpSnd (HP (_, pf)) = pf

-- * Zip Proofs

data ZipProof f = ZP (Proof f) (ZipPath f)

data ZipPath f = Top | Step (Sequent f) RuleName (ZipPath f) [Proof f] [Proof f]

instance HasHistory ZipPath where
  histOf :: ZipPath f -> History f
  histOf Top = []
  histOf (Step xs _ p _ _) = xs : histOf p

instance HasHistory ZipProof where
  histOf :: ZipProof f -> History f
  histOf (ZP _ zpath) = histOf zpath

-- * Tree-based prover

startForT :: f -> ProofWithH f
startForT f =  HP ([], Node (Set.singleton (Right f)) Nothing)

isClosedPfT :: Eq f => ProofWithH f -> Bool
isClosedPfT (HP (_,fs)) = isClosedPf fs

extendT  :: (Eq f, Show f, Ord f) => Logic f -> ProofWithH f -> [ProofWithH f]
extendT l pt@(HP (h, Node fs Nothing)) =
  case ( List.filter (isApplicableRule h fs) (safeRules l)
       , unsafeRules l ) of
  -- The safe rule r can be applied:
  (r:_ , _       ) ->
    [ HP (h, Node fs (Just (therule, map hpSnd ts)))
    | (therule, result) <- r h fs f
    , ts <- pickOneOfEach [ extendT l (HP (fs : h, Node newSeqs Nothing))
                          | newSeqs <- result ] ]
    where f = Set.elemAt 0 $ Set.filter (\g -> isApplicable h fs g r) fs
              -- (Using the first possible active formula.)
  -- At least one unsafe rule can be applied:
  ([], rs@(_:_)) -> List.concatMap applyRule rs
    where
      applyRule r = case List.filter isClosedPfT nps of
                    tp : _ -> [tp]
                    [] -> [HP (h, Node fs Nothing)]
        -- | any isClosedPfT nps = List.take 1 (List.filter isClosedPfT nps)
        -- | otherwise = [HP (h, Node fs Nothing)]
        where
          gs = Set.filter (\g -> isApplicable h fs g r) fs
          nps = concat $ List.concatMap tryExtendT gs
          tryExtendT g = [ List.map (\pwh -> HP (h, Node fs (Just (therule, [hpSnd pwh]))))
                           $ extendT l (HP (fs : h, Node (head result) Nothing))
                           -- (Using head because we never have branching unsafeRules.)
                         | (therule, result) <- r (histOf pt) fs g ]
  -- No rule can be applied, leave proof unfinished:
  ([], []      ) -> [HP (h, Node fs Nothing)]
extendT _ (HP (_,Node _ (Just _))) = error "already extended"

proveT :: (Eq f, Show f,Ord f) => Logic f -> f -> [Proof f]
proveT l f = List.map hpSnd $ extendT l (startForT f)

isProvableT :: (Eq f, Show f, Ord f) => Prover f
isProvableT l f = any isClosedPf (proveT l f)

proveprintT :: (Eq f, Show f,Ord f) => Logic f -> f -> Proof f
proveprintT l f = if isProvableT l f
                then head $ List.filter isClosedPf (proveT l f)
                else head (proveT l f)

provePdfT :: (Ord f,Show f, Eq f) => Logic f -> f -> IO FilePath
provePdfT l f= pdf $ proveprintT l f

-- * Zipper-based prover

instance TreeLike ZipProof where
  zsingleton x                               = ZP (Node (Set.singleton (Right x)) Nothing) Top
  move_left (ZP c (Step s r p (x:xs) ys))    = ZP x (Step s r p xs (c:ys))
  move_left _                                = error "cannot go left"
  move_right (ZP c (Step s r p xs (y:ys)))   = ZP y (Step s r p (c:xs) ys)
  move_right _                               = error "cannot go right"
  move_up (ZP c (Step s r p xs ys))          = ZP (Node s (Just (r, (c:xs) ++ ys))) p
  move_up _                                  = error "cannot go up"
  move_down (ZP (Node s (Just (r, x:xs))) p) = ZP x (Step s r p [] xs)
  move_down _                                = error "cannot go down"
  zdelete (ZP _ (Step s _ Top _ _))          = ZP (Node s Nothing) Top
  zdelete (ZP _ (Step s _ p _ _))            = ZP (Node s Nothing) p
  zdelete _                                  = error "cannot delete top"

fromZip :: ZipProof f -> Proof f
fromZip (ZP x Top) = x
fromZip zp = fromZip (move_up zp)

-- | Does the node have a right sibling?
hasRsibi :: ZipPath f -> Bool
hasRsibi (Step _ _ _ _ (_:_))= True
hasRsibi _ = False

-- | Switch path, left-biased
switch :: ZipProof f -> ZipProof f
switch (ZP pf Top) = ZP pf Top
switch (ZP pf p) = if hasRsibi p
                      then move_right (ZP pf p)
                      else switch.move_up $ ZP pf p

startForZ :: f -> ZipProof f
startForZ f = ZP (Node (Set.singleton (Right f)) Nothing) Top

isClosedZP :: Eq f => ZipProof f -> Bool
isClosedZP (ZP fs Top) = isClosedPf fs
isClosedZP (ZP fs p) = isClosedPf fs &&  (isClosedZP . switch $ ZP fs p)

extendZ  :: (Ord f,Eq f) => Logic f -> ZipProof f -> [ZipProof f]
extendZ l zp@(ZP (Node fs Nothing) p) =
  case ( List.filter (isApplicableRule (histOf zp) fs) (safeRules l)
       , unsafeRules l) of
  -- The safe rule r can be applied:
  (r:_ , _       )    ->  let f = Set.elemAt 0 $ Set.filter (\g -> isApplicable (histOf zp) fs g r) fs
                              (therule,results) = head $ r (histOf zp) fs f
                              newZP         = ZP (Node fs (Just (therule, [Node newSeq Nothing | newSeq <- results]))) p
                              nextZP
                                | null results = switch    newZP -- no children, i.e. proved
                                | otherwise   = move_down newZP
                          in extendZ l nextZP
  -- At least one unsafe rule can be applied:
  ([], rs@(_:_))    -> List.concatMap applyRule rs
    where
      applyRule r = case List.filter isClosedZP nps of
                    np : _ -> [np]
                    [] -> [ZP (Node fs Nothing) p]
        -- | any isClosedZP nps = List.take 1 (List.filter isClosedZP nps)
        -- | otherwise = [ZP (Node fs Nothing) p]
        where
          gs = Set.filter (\g -> isApplicable (histOf zp) fs g r) fs
          nps = concat $ List.concatMap tryExtendZ gs
          tryExtendZ g = [ extendZ l (ZP (Node (head result) Nothing) (Step fs therule p [] []) )
                         | (therule,result) <- r (histOf zp) fs g ]
  -- No rule can be applied, leave proof unfinished:
  ([], []      )    -> [ZP (Node fs Nothing) p]

extendZ _ zp@(ZP (Node _ (Just _ )) _) = [zp] -- needed after switch

proveZ :: (Eq f, Ord f) => Logic f -> f -> [Proof f]
proveZ l f = List.map fromZip $ extendZ l (startForZ f)

isProvableZ :: (Eq f, Ord f) => Prover f
isProvableZ l f = any isClosedZP $ extendZ l (startForZ f)

proveprintZ :: (Eq f, Ord f) => Logic f -> f -> Proof f
proveprintZ l f = if isProvableZ l f
                then head $  List.filter isClosedPf (proveZ l f)
                else head (proveZ l f)

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

instance (Show f,Ord f) => (DispAble (Proof f)) where
  toGraph = toGraph' "" where
    toGraph' pref (Node fs Nothing) = do
      node pref [shape PlainText, toLabel $ ppSeq fs]
      node (pref ++ "open") [shape PlainText, toLabel "?"]
      edge pref (pref ++ "open") []
    toGraph' pref (Node fs (Just (rule',ts))) = do
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
      post = "\\end{document}"
    writeFile "temp.tex" (pre ++ tex x ++ post)
    (_inp, _out, err, pid) <- runInteractiveCommand "pdflatex -interaction=nonstopmode temp.tex"
    _ <- waitForProcess pid
    hGetContents err >>= (\e -> unless (null e) (putStrLn e))

instance (Ord f, TeX f) => TeX (Sequent f) where
  tex xs = "\\ensuremath{" ++ texList (Set.toList $ leftsSet xs) ++ " \\Rightarrow " ++ texList (Set.toList $ rightsSet xs) ++ "} "

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
    '☐' -> "\\Box"
    c -> [c]

-- | General LaTeX code to show a proof using the buss package.
toBuss :: (Show f, TeX f, Ord f) => Proof f -> String
toBuss p = toB p ++ "\\DisplayProof\n" where
  toB (Node fs Nothing) = "\\AxiomC{" ++ tex fs ++ " }"
  toB (Node fs (Just (rule', ts))) =
    concatMap toB ts
    ++
    case length ts of
    0 -> "\\AxiomC{\\phantom{I}}\n " ++ r ++ "\\UnaryInfC{ " ++ tex fs ++  "}\n"
    1 -> r ++ "\\UnaryInfC{ " ++ tex fs ++  "}\n"
    2 -> r ++ "\\BinaryInfC{ " ++ tex fs ++  "}\n"
    _ -> error "too many premises"
    where r = "\\LeftLabel{" ++ texRuleName rule' ++ "}\n"

instance (Show f, TeX f, Ord f) => TeX (Proof f) where
  tex = toBuss

-- * The Propositional language

type Atom = Char

-- | Propositional Formulas
data FormP = BotP | AtP Atom | ConP FormP FormP | DisP FormP FormP | ImpP FormP FormP
  deriving (Eq,Ord,Generic)

isatomP :: FormP -> Bool
isatomP (AtP _) = True
isatomP _ = False

isAxiomP :: Rule FormP
isAxiomP _ fs _ = [ ("ax", [])
                  | any (\f -> isatomP f && Right f `Set.member` fs) (leftsSet fs) ]

leftBotP :: Rule FormP
leftBotP _ fs _ = [ ("⊥L", []) | Left BotP `Set.member` fs ]

negP :: FormP -> FormP
negP f = ImpP f BotP

topP :: FormP
topP = negP BotP

iffP :: FormP -> FormP -> FormP
iffP f g = ConP (ImpP f g) (ImpP g f)

instance Show FormP where
  show BotP       = "⊥"
  show (AtP a)    = [a]
  show (ConP f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
  show (DisP f g) = "(" ++ show f ++ " v " ++ show g ++ ")"
  show (ImpP f g) = "(" ++ show f ++ " → " ++ show g ++ ")"

instance TeX FormP where
  tex BotP       = "\\bot"
  tex (AtP a)    = [a]
  tex (ConP f g) = "(" ++ tex f ++ " \\land " ++ tex g ++ ")"
  tex (DisP f g) = "(" ++ tex f ++ " \\lor " ++ tex g ++ ")"
  tex (ImpP f g) = "(" ++ tex f ++ " \\to " ++ tex g ++ ")"

instance Arbitrary FormP where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotP, AtP <$> choose ('p','t') ]
    genForm 1 = AtP <$> choose ('p','t')
    genForm n = oneof
      [ pure BotP
      , AtP <$> choose ('p','t')
      , ImpP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , DisP <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      ]
  shrink = nub . genericShrink

-- * The Modal Language

data FormM = BotM | AtM Atom | ConM FormM FormM | DisM FormM FormM | ImpM FormM FormM | Box FormM
  deriving (Eq,Ord,Generic)

isatomM :: FormM -> Bool
isatomM (AtM _) = True
isatomM _ = False

isAxiomM :: Rule FormM
isAxiomM _ fs _ = [ ("ax", [])
                  | any (\f -> isatomM f && Right f `Set.member` fs) (leftsSet fs) ]

leftBotM :: Rule FormM
leftBotM _ fs _ = [ ("L⊥", [])
                  | Left BotM `Set.member` fs ]

negM :: FormM -> FormM
negM f = ImpM f BotM

topM :: FormM
topM = negM BotM

iffM :: FormM -> FormM -> FormM
iffM f g = ConM (ImpM f g) (ImpM g f)

diaM :: FormM -> FormM
diaM f = negM $ Box $ negM f

instance Show FormM where
  show BotM       = "⊥"
  show (AtM a)    = [a]
  show (ConM f g) = "(" ++ show f ++ " ∧ " ++ show g ++ ")"
  show (DisM f g) = "(" ++ show f ++ " v " ++ show g ++ ")"
  show (ImpM f g) = "(" ++ show f ++ " → " ++ show g ++ ")"
  show (Box f)    = "☐" ++ show f

instance TeX FormM where
  tex BotM       = "\\bot"
  tex (AtM a)    = [a]
  tex (ConM f g) = "(" ++ tex f ++ " \\land " ++ tex g ++ ")"
  tex (DisM f g) = "(" ++ tex f ++ " \\lor " ++ tex g ++ ")"
  tex (ImpM f g) = "(" ++ tex f ++ " \\to " ++ tex g ++ ")"
  tex (Box f)    = " \\Box " ++ tex f

instance Arbitrary FormM where
  arbitrary = sized genForm where
    factor = 2
    genForm 0 = oneof [ pure BotM, AtM <$> choose ('p','t') ]
    genForm 1 = AtM <$> choose ('p','t')
    genForm n = oneof
      [ pure BotM
      , AtM <$> choose ('p','t')
      , ImpM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , ConM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , DisM <$> genForm (n `div` factor) <*> genForm (n `div` factor)
      , Box <$> genForm (n `div` factor)
      ]
  shrink = nub . genericShrink

-- * Embedding Propositional language into Modal language

pTom :: FormP -> FormM
pTom BotP = BotM
pTom (AtP x) = AtM x
pTom (ConP x y) = ConM (pTom x) (pTom y)
pTom (DisP x y) = DisM (pTom x) (pTom y)
pTom (ImpP x y) = ImpM (pTom x) (pTom y)

-- The Gödel–McKinsey–Tarski Translation
translation :: FormP -> FormM
translation BotP = BotM
translation (AtP x) = Box $ AtM x
translation (ConP x y) = ConM (translation x) (translation y)
translation (DisP x y) = DisM (translation x) (translation y)
translation (ImpP x y) = Box $ ImpM (translation x) (translation y)
