{
{-# OPTIONS_GHC -w #-}
{-# LANGUAGE OverloadedStrings #-}
module FormM.Parse where

import Data.String( IsString(..) )

import FormM.Token
import FormM.Lex

import General
}

%name parseFormM FormM
%tokentype { Token AlexPosn }
%error { parseError }

%monad { ParseResult } { >>= } { Right }

%token
  TOP    { TokenTop    _ }
  BOT    { TokenBot    _ }
  'p'    { TokenP      _ }
  '('    { TokenOB     _ }
  ')'    { TokenCB     _ }
  '<>'   { TokenDia    _ }
  '[]'   { TokenBox    _ }
  '&'    { TokenCon    _ }
  '|'    { TokenDis    _ }
  '=>'   { TokenImpl   _ }
  '<->'  { TokenEqui   _ }
  '~'    { TokenNeg    _ }
  INT    { TokenInt $$ _ }

%left '|'
%left '&'
%right '=>'
%left '[]'
%left '<>'
%left '~'

%nonassoc 'p'

%%

-- data FormM = BotM | AtM Atom | ConM FormM FormM | DisM FormM FormM | ImpM FormM FormM | Box FormM

FormM : TOP { topM }
     | BOT { BotM }
     | '(' FormM ')' { $2 }
     | '~' FormM { negM $2 }
     | FormM '=>'  FormM { ImpM $1 $3 }
     | FormM '&'   FormM { ConM $1 $3 }
     | FormM '|'   FormM { DisM $1 $3 }
     | FormM '<->' FormM { iffM $1 $3 }
     | 'p' INT { AtM $2 }
     | '[]' FormM { Box $2 }
     | '<>' FormM { diaM $2 }

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

instance IsString General.FormM where
  fromString s = case parseFormM (alexScanTokens s) of
    Left e  -> error ("Error at " ++ show e ++ " when parsing " ++ s ++ " \n")
    Right f -> f
}
