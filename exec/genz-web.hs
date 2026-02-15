{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Prelude
import Data.FileEmbed
import Data.Maybe
import Web.Scotty
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Language.Javascript.JQuery as JQuery
import Language.Haskell.TH.Syntax
import Network.Wai.Handler.Warp (defaultSettings, setHost, setPort)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import General
import FormP
import FormM
import General.Lex
import FormM.Parse (parseFormM)
import FormP.Parse (parseFormP)

import qualified Logic.Propositional.CPL as CPL
import qualified Logic.Propositional.IPL as IPL
import qualified Logic.Modal.D as D
import qualified Logic.Modal.D4 as D4
import qualified Logic.Modal.D45 as D45
import qualified Logic.Modal.GL as GL
import qualified Logic.Modal.K as K
import qualified Logic.Modal.K4 as K4
import qualified Logic.Modal.K45 as K45
import qualified Logic.Modal.S4 as S4
import qualified Logic.Modal.T as T

main :: IO ()
main = do
  putStrLn "GenZ web"
  port <- fromMaybe 3300 . (readMaybe =<<) <$> lookupEnv "PORT"
  path <- fromMaybe "/" <$> lookupEnv "WEBPATH"
  putStrLn $ "Please open this link: http://127.0.0.1:" ++ show port ++ path
  let mySettings = Options 1 (setHost "127.0.0.1" $ setPort port defaultSettings)
  let index = html . TL.fromStrict $ embeddedFile "index.html"
  scottyOpts mySettings $ do
    get (capture path) index
    get (capture $ path ++ "index.html") index
    get (capture $ path ++ "jquery.js") . (\t -> addHeader "Content-Type" "text/javascript" >> html t) . TL.fromStrict $ embeddedFile "jquery.js"
    post (capture $ path ++ "prove") $ do
      textinput <- param "textinput"
      let lexResult = alexScanTokensSafe textinput
      output <- case lexResult of
        Left (_,col) -> return
          [ "<pre>INPUT: " ++ textinput ++ "</pre>"
          , "<pre>" ++ replicate (col + length ("INPUT:" :: String)) ' ' ++ "^</pre>"
          , "<pre>Lexing error in column " ++ show col ++ ".</pre>" ]
        Right tokenList -> do
          useProp <- (== ("prop" :: String)) <$> param "syntax"
          s_logic <- param "logic"
          s_struct <- param "struct"
          let myParserLogic = if useProp
                then (fmap Left . parseFormP, Left (propLogic s_logic))
                else (fmap Right . parseFormM, Right (modLogic s_logic))
          case fst myParserLogic tokenList of
            Left (_,col) ->
              return
              [ "<pre>INPUT: " ++ textinput ++ "</pre>"
              , "<pre>" ++ replicate (col + length ("INPUT:" :: String)) ' ' ++ "^</pre>"
              , "<pre>Parse error in column " ++ show col ++ ".</pre>" ]
            Right lr_frm ->
              return $ webProveWrap (snd myParserLogic) lr_frm s_struct
      html $ mconcat $ map TL.pack output

propLogic :: String -> Logic FormP
propLogic s = case s of "CPL" -> CPL.classical
                        "IPL" -> IPL.intui
                        _ -> error $ "Unknown propositional logic: " ++ s

modLogic :: String -> Logic FormM
modLogic s = case s of
                      "D"  -> D.d
                      "D4" -> D4.dfour
                      "D45"-> D45.dfourfive
                      "GL" -> GL.gl
                      "K" -> K.k
                      "K4" -> K4.kfour
                      "K45" -> K45.kfourfive
                      "S4" -> S4.sfour
                      "T"  -> T.t
                      _ -> error $ "Unknown modal logic: " ++ s

webProveWrap :: Either (Logic FormP) (Logic FormM) -> Either FormP FormM -> String -> [String]
webProveWrap (Left l) (Left f) = webProve l f
webProveWrap (Right l) (Right f) = webProve l f
webProveWrap _ _ = error "Wrong combination of logic and syntax."

webProve :: (Eq f, Ord f, Show f, TeX f) => Logic f -> f -> String -> [String]
webProve logic frm struct =
  let (isPrv, prv) = case struct of
        "zipper" -> (isProvableZ, proofZ)
        "tree" -> (isProvableT, proofT)
        _ -> error $ "Unknown data structure: " ++ struct
      p_tex = case prv logic frm of
        Nothing -> ""
        Just p1 -> tex p1
  in
    [ "<pre>Parsed input: " ++ show frm ++ "</pre>" -- TODO pretty? tex?
    , if isPrv logic frm
        then "PROVED. <style type='text/css'> #output { border-color: green; } </style>\n"
        else "NOT proved. <style type='text/css'> #output { border-color: red; } </style>\n"
    , if p_tex /= "" then "<div align='center'>\\( \\begin{prooftree}" ++ p_tex ++ " \\end{prooftree} \\)</div>" else ""
    ]

embeddedFile :: String -> T.Text
embeddedFile str = case str of
  "index.html" -> E.decodeUtf8 $(embedFile "exec/index.html")
  "jquery.js"  -> E.decodeUtf8 $(embedFile =<< runIO JQuery.file)
  _            -> error "File not found."
