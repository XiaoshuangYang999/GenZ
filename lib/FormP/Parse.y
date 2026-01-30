{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module FormP.Parse where

import Data.String( IsString(..) )
import Data.Char
import Data.List

import General.Token
import General.Lex
import FormP
import General
}

%name parseFormP FormP
%tokentype { Token AlexPosn }
%error { parseError }

%monad { ParseResult } { >>= } { Right }

%token
  TOP    { TokenTop    _ }
  BOT    { TokenBot    _ }
  '('    { TokenOB     _ }
  ')'    { TokenCB     _ }
  '&'    { TokenCon    _ }
  '|'    { TokenDis    _ }
  '=>'   { TokenImpl   _ }
  '<->'  { TokenEqui   _ }
  '~'    { TokenNeg    _ }
  STR    { TokenString $$ _ }

%right '<->'
%right '=>'
%left '|'
%left '&'
%left '~'

%%

FormP : TOP { top }
     | BOT { BotP }
     | '(' FormP ')' { $2 }
     | '~' FormP { neg $2 }
     | FormP '=>'  FormP { ImpP $1 $3 }
     | FormP '&'   FormP { ConP $1 $3 }
     | FormP '|'   FormP { DisP $1 $3 }
     | FormP '<->' FormP { iff $1 $3 }
     | STR { AtP $1 }

{
type ParseResult a = Either (Int,Int) a

parseError :: [Token AlexPosn] -> ParseResult a
parseError []     = Left (1,1)
parseError (t:ts) = Left (lin,col)
  where (AlexPn _ lin col) = apn t

scanParseSafe :: _ -> String -> ParseResult a
scanParseSafe pfunc input =
  case alexScanTokensSafe input of
    Left pos        -> Left pos
    Right lexResult -> case pfunc lexResult of
      Left pos -> Left pos
      Right x  -> Right x

instance IsString FormP where
  fromString s = case parseFormP (alexScanTokens s) of
    Left e  -> error ("Error at " ++ show e ++ " when parsing " ++ s ++ " \n")
    Right f -> f

-- NOTE: This tptp parser was assisted by ChatGPT
-- | Parse .tptp file into a single formula: (A1 & ... & An) -> C
--   where each Ai is a formula with role = axiom, and C is the first conjecture.
parseTPTPProblem :: String -> ParseResult FormP
parseTPTPProblem s = do
  let blocks  = fofBlocks s                   -- each fof(...) block
      triples = map parseFOFHeader blocks     -- [(name,role,formulaText)]
      axTexts = [ f | (_,r,f) <- triples, r == "axiom" ]
      conTexts = [ f | (_,r,f) <- triples, r == "conjecture" ]

  cText <- case conTexts of
             []    -> Left (0,0)              -- no conjecture found
             (x:_) -> Right x

  -- Parse each formula using the existing FormP parser
  as <- mapM (scanParseSafe parseFormP . normalizeFormula) axTexts
  c  <-        scanParseSafe parseFormP (normalizeFormula cText)

  case as of
    []       -> Right c
    (a:rest) -> Right (ImpP (foldl ConP a rest) c)

-- | Split the whole file into lines and extract complete fof(...) blocks
--   (each block may span multiple lines).
fofBlocks :: String -> [String]
fofBlocks = go [] [] . lines
  where
    go acc cur [] =
      reverse (if null cur then acc else unlines (reverse cur) : acc)

    go acc cur (l:ls)
      -- Skip comment lines
      | "%" `isPrefixOf` dropWhile isSpace l =
          go acc cur ls

      -- Start of a new fof block: line begins with "fof("
      | "fof(" `isPrefixOf` dropWhile isSpace l =
          let acc' = if null cur
                        then acc
                        else unlines (reverse cur) : acc
          in go acc' [l] ls

      -- No fof started yet; keep looking
      | null cur =
          go acc cur ls

      -- End of current fof block: line contains "))."
      | "))." `isInfixOf` l =
          go (unlines (reverse (l:cur)) : acc) [] ls

      -- Middle line of the current fof block
      | otherwise =
          go acc (l:cur) ls

-- | Extract (name, role, formulaText) from a fof(...) block.
--   Input is roughly of the form:
--   fof(axiom1,axiom,( ( p1 <=> p2 ) => ( p1 & p2 ) )).
parseFOFHeader :: String -> (String, String, String)
parseFOFHeader block =
  let flat  = unwords (words block)          -- flatten to a single line, normalize whitespace
      flat' = dropWhile isSpace flat
      -- Drop the leading "fof("
      rest1 = dropPrefix "fof(" flat'
      (name, rest2)     = break (== ',') rest1
      rest3             = drop 1 rest2      -- drop first comma
      (rolePart, rest4) = break (== ',') rest3
      role              = trim rolePart
      rest5             = drop 1 rest4      -- after second comma we expect "( ... )."
      -- Strip the trailing ")." (fof terminator), keeping the formula's final ')'
      (fmlPart, _)      = breakOn ")." rest5
      formulaText       = trim fmlPart
  in (trim name, role, formulaText)

-- | Map TPTP boolean constants to the identifiers understood by our lexer (in case they appear).
normalizeFormula :: String -> String
normalizeFormula =
      replace "$false" "false"
    . replace "$true"  "true"

-- | Small helper functions
trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropPrefix :: String -> String -> String
dropPrefix pre s
  | pre `isPrefixOf` s = drop (length pre) s
  | otherwise          = s

-- Split the string at the first occurrence of the given substring.
-- Returns (prefix, suffix-without-the-substring).
breakOn :: String -> String -> (String, String)
breakOn pat = go []
  where
    go acc "" = (reverse acc, "")
    go acc s@(c:cs)
      | pat `isPrefixOf` s = (reverse acc, drop (length pat) s)
      | otherwise          = go (c:acc) cs

replace :: String -> String -> String -> String
replace old new = go
  where
    go [] = []
    go s@(c:cs)
      | old `isPrefixOf` s = new ++ go (drop (length old) s)
      | otherwise          = c : go cs

-- Debug helper: print the (name, role, formulaText) triples extracted from a tptp file.
debugTPTP :: String -> IO ()
debugTPTP s = do
  let blocks  = fofBlocks s
      triples = map parseFOFHeader blocks
  putStrLn "Blocks/parsing result:"
  mapM_ print triples
}
