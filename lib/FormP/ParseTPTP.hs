module FormP.ParseTPTP where

import Data.Char
import Data.List

import FormP
import FormP.Parse

-- NOTE: This tptp parser was assisted by ChatGPT

-- NOTE by MG: better would be to properly write a TPTP grammar into Parse.y.

rewriteTPTPProblem :: String -> Either (Int,Int) String
rewriteTPTPProblem s = do
  let blocks  = fofBlocks s                   -- each fof(...) block
      triples = map parseFOFHeader blocks     -- [(name,role,formulaText)]
      axTexts = [ f | (_,label,f) <- triples, label == "axiom" ]
      conTexts = [ f | (_,label,f) <- triples, label == "conjecture" ]
  cText <- case conTexts of
             []    -> Left (0,0)              -- no conjecture found
             (x:_) -> Right x
  case axTexts of
    []       -> Right cText
    (a:rest) -> Right (foldl (\ fs gs -> fs ++ " & " ++ gs) a rest ++ " -> " ++ cText)

-- NOTE by MG: The function `parseTPTPProblem` does too much, for the executable we
-- only want a `String -> String` before using the existing FormP parser.

-- | Parse .tptp file into a single formula: (A1 & ... & An) -> C
--   where each Ai is a formula with role = axiom, and C is the first conjecture.
parseTPTPProblem :: String -> ParseResult FormP
parseTPTPProblem s = do
  let blocks  = fofBlocks s                   -- each fof(...) block
      triples = map parseFOFHeader blocks     -- [(name,role,formulaText)]
      axTexts = [ f | (_,label,f) <- triples, label == "axiom" ]
      conTexts = [ f | (_,label,f) <- triples, label == "conjecture" ]

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


-- DEBUGGING

-- Debug helper: print the (name, role, formulaText) triples extracted from a tptp file.
debugTPTP :: String -> IO ()
debugTPTP s = do
  let blocks  = fofBlocks s
      triples = map parseFOFHeader blocks
  putStrLn "Blocks/parsing result:"
  mapM_ print triples
