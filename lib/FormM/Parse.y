{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module FormM.Parse where

import Data.String( IsString(..) )

import General.Token
import General.Lex
import FormM
import General
}

%name parseFormM FormM
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
  '<'    { TokenDiaL   _ }
  '>'    { TokenDiaR   _ }
  '['    { TokenBoxL   _ }
  ']'    { TokenBoxR   _ }
  '[]'   { TokenBox    _ }
  '<>'   { TokenDia    _ }
  STR    { TokenString $$ _ }

%right '<->'
%right '=>'
%left '|'
%left '&'
%left '~'

%nonassoc '[' ']' '<' '>' '[]'

%%

FormM : TOP { top }
     | BOT { BotM }
     | '(' FormM ')' { $2 }
     | '~' FormM { neg $2 }
     | FormM '=>'  FormM { ImpM $1 $3 }
     | FormM '&'   FormM { ConM $1 $3 }
     | FormM '|'   FormM { DisM $1 $3 }
     | FormM '<->' FormM { iff $1 $3 }
     | STR { AtM $1 }
     -- TODO multi-modal?
     | '[]' FormM { Box $2 }
     | '[' ']' FormM { Box $3 }
     | '[' STR ']' FormM { Box $4 }
     | '<>' FormM { dia $2 }
     | '<' '>' FormM { dia $3 }
     | '<' STR '>' FormM { dia $4 }

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

instance IsString FormM where
  fromString s = case parseFormM (alexScanTokens s) of
    Left e  -> error ("Error at " ++ show e ++ " when parsing " ++ s ++ " \n")
    Right f -> f
}
