{
{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-tabs -fno-warn-missing-signatures #-}
module General.Lex where

import General.Token
}

%wrapper "posn"

$dig = 0-9      -- digits
$alf = [a-zA-Z] -- alphabetic characters

tokens :-
  -- ignore whitespace:
  $white+           ;
  -- ignore the word "begin"
  "begin"           ;
  -- ignore the word "end"
  "end"             ;
  -- keywords and punctuation:
  "("               { \ p _ -> TokenOB                p }
  ")"               { \ p _ -> TokenCB                p }
  -- Formulas:
  "true"            { \ p _ -> TokenTop               p }
  "⊤"               { \ p _ -> TokenTop               p }
  "false"           { \ p _ -> TokenBot               p }
  "⊥"               { \ p _ -> TokenBot               p }
  "~"               { \ p _ -> TokenNeg               p }
  "¬"               { \ p _ -> TokenNeg               p }
  "&"               { \ p _ -> TokenCon               p }
  "|"               { \ p _ -> TokenDis               p }
  "v"               { \ p _ -> TokenDis               p }
  "=>"              { \ p _ -> TokenImpl              p }
  "→"               { \ p _ -> TokenImpl              p }
  "->"              { \ p _ -> TokenImpl              p }
  "-->"             { \ p _ -> TokenImpl              p }
  "<->"             { \ p _ -> TokenEqui              p }
  "<=>"             { \ p _ -> TokenEqui              p }
  "<-->"            { \ p _ -> TokenEqui              p }
  "↔"               { \ p _ -> TokenEqui              p }
  "◇"               { \ p _ -> TokenDia               p }
  "♢"               { \ p _ -> TokenDia               p }
  "<"               { \ p _ -> TokenDiaL              p }
  ">"               { \ p _ -> TokenDiaR              p }
  "☐"               { \ p _ -> TokenBox               p }
  "◻"               { \ p _ -> TokenBox               p }
  "□"               { \ p _ -> TokenBox               p }
  "[]"              { \ p _ -> TokenBox               p }
  "["               { \ p _ -> TokenBoxL              p }
  "]"               { \ p _ -> TokenBoxR              p }
  -- Strings:
  [$alf $dig _]+      { \ p s -> TokenString s          p }
  -- Special chars:
  "𝑝"               { \ p _ -> TokenString "p"        p }

{
type LexResult a = Either (Int,Int) a

alexScanTokensSafe :: String -> LexResult [Token AlexPosn]
alexScanTokensSafe str = go (alexStartPos,'\n',[],str) where
  go inp@(pos,_,_,str) =
    case (alexScan inp 0) of
      AlexEOF -> Right []
      AlexError ((AlexPn _ line column),_,_,_) -> Left (line,column)
      AlexSkip  inp' len     -> go inp'
      AlexToken inp' len act -> case (act pos (take len str), go inp') of
        (_, Left lc) -> Left lc
        (x, Right y) -> Right (x : y)
}
