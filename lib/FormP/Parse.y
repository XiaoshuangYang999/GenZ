{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module FormP.Parse where

import Data.String( IsString(..) )

import General.Token
import General.Lex
import FormP
import General

import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf)
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

-- ========= TPTP FOF front-end（简化、只处理命题 FOF） =========

-- | 从一个 .tptp 文件的内容得到单个公式：
--   (A1 & ... & An) -> C
--   其中 Ai 是 role = axiom 的公式，C 是第一个 conjecture。
parseTPTPProblem :: String -> ParseResult FormP
parseTPTPProblem s = do
  let blocks  = fofBlocks s                   -- 每个 fof(...) 块
      triples = map parseFOFHeader blocks     -- [(name,role,formulaText)]
      axTexts = [ f | (_,r,f) <- triples, r == "axiom" ]
      conTexts = [ f | (_,r,f) <- triples, r == "conjecture" ]

  cText <- case conTexts of
             []    -> Left (0,0)              -- 没有 conjecture
             (x:_) -> Right x

  -- 用你现有的 parser 解析每个公式
  as <- mapM (scanParseSafe parseFormP . normalizeFormula) axTexts
  c  <-        scanParseSafe parseFormP (normalizeFormula cText)

  case as of
    []       -> Right c
    (a:rest) -> Right (ImpP (foldl ConP a rest) c)

-- | 把整个文件按行切开，提取每个 fof(...) 的完整块（可能跨多行）
fofBlocks :: String -> [String]
fofBlocks = go [] [] . lines
  where
    go acc cur [] =
      reverse (if null cur then acc else unlines (reverse cur) : acc)

    go acc cur (l:ls)
      -- 跳过注释行
      | "%" `isPrefixOf` dropWhile isSpace l =
          go acc cur ls

      -- 新的 fof 开头：fof(
      | "fof(" `isPrefixOf` dropWhile isSpace l =
          let acc' = if null cur
                        then acc
                        else unlines (reverse cur) : acc
          in go acc' [l] ls

      -- 还没开始任何 fof，继续找
      | null cur =
          go acc cur ls

      -- 当前 fof 结束行：包含 "))."
      | "))." `isInfixOf` l =
          go (unlines (reverse (l:cur)) : acc) [] ls

      -- 当前 fof 的中间部分
      | otherwise =
          go acc (l:cur) ls

-- | 从一个 fof(...) 块里，抽出 (name, role, formulaText)
--   输入大致形如：fof(axiom1,axiom,( ( p1 <=> p2 ) => ( p1 & p2 ) )).
parseFOFHeader :: String -> (String, String, String)
parseFOFHeader block =
  let flat  = unwords (words block)          -- 压成一行，去掉多余空白
      flat' = dropWhile isSpace flat
      -- 去掉前面的 "fof("
      rest1 = dropPrefix "fof(" flat'
      (name, rest2)     = break (== ',') rest1
      rest3             = drop 1 rest2      -- 去掉第一个逗号
      (rolePart, rest4) = break (== ',') rest3
      role              = trim rolePart
      rest5             = drop 1 rest4      -- 第二个逗号后面应该是 "( ... ))."
      -- 去掉末尾的 "))."
      (fmlPart, _)      = breakOn ")." rest5
      formulaText       = trim fmlPart
  in (trim name, role, formulaText)

-- | 把 TPTP 中的布尔常量换成你 lexer 能认的单词（如果将来遇到的话）
normalizeFormula :: String -> String
normalizeFormula =
      replace "$false" "false"
    . replace "$true"  "true"

-- ===== 一些小工具函数 =====

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

dropWhileEnd :: (Char -> Bool) -> String -> String
dropWhileEnd p = reverse . dropWhile p . reverse

dropPrefix :: String -> String -> String
dropPrefix pre s
  | pre `isPrefixOf` s = drop (length pre) s
  | otherwise          = s

-- 在字符串里按子串切分：返回 (前缀, 去掉 pat 后的剩余)
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

-- 这两个为了不再 import Data.List.extra
isInfixOf :: String -> String -> Bool
isInfixOf pat s =
  any (pat `isPrefixOf`) (tails s)

tails :: [a] -> [[a]]
tails [] = [[]]
tails xs@(_:xs') = xs : tails xs'

debugTPTP :: String -> IO ()
debugTPTP s = do
  let blocks  = fofBlocks s
      triples = map parseFOFHeader blocks
  putStrLn "Blocks/parsing result:"
  mapM_ print triples
}