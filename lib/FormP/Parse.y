{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module FormP.Parse where

import Data.String( IsString(..) )

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
}
