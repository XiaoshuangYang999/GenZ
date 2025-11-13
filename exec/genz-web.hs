{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (main) where

import Prelude
import Data.FileEmbed
import Data.Maybe (fromMaybe)
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
import General.Lex
import FormM.Parse (parseFormM)
import FormP.Parse (parseFormP)

import K
import IPL

main :: IO ()
main = do
  putStrLn "GenZ web"
  port <- fromMaybe 3300 . (readMaybe =<<) <$> lookupEnv "PORT"
  path <- fromMaybe "/" <$> lookupEnv "WEBPATH"
  putStrLn $ "Please open this link: http://127.0.0.1:" ++ show port ++ path
  let mySettings = Options 1 (setHost "127.0.0.1" $ setPort port defaultSettings)
  let index = html . TL.fromStrict $ embeddedFile "index.html"
  scottyOpts mySettings $ do
    get (capture $ path) index
    get (capture $ path ++ "/") index
    get (capture $ path ++ "/index.html") index
    get (capture $ path ++ "/jquery.js") . (\t -> addHeader "Content-Type" "text/javascript" >> html t) . TL.fromStrict $ embeddedFile "jquery.js"
    post (capture $ path ++ "/prove") $ do
      logic <- param "logic"
      textinput <- param "textinput"
      let lexResult = alexScanTokensSafe textinput
      html $ mconcat $ map TL.pack $ case lexResult of
        Left (_,col) ->
          [ "<pre>INPUT: " ++ textinput ++ "</pre>"
          , "<pre>" ++ replicate (col + length ("INPUT:" :: String)) ' ' ++ "^</pre>"
          , "<pre>Lexing error in column " ++ show col ++ ".</pre>" ]
        Right tokenList -> do

          let myParser = if (logic :: String) == "K" then fmap Left . parseFormM else fmap Right . parseFormP
          case myParser tokenList of
            Left (_,col) ->
              [ "<pre>INPUT: " ++ textinput ++ "</pre>"
              , "<pre>" ++ replicate (col + length ("INPUT:" :: String)) ' ' ++ "^</pre>"
              , "<pre>Parse error in column " ++ show col ++ ".</pre>" ]
            Right m_or_p_f ->
              let (f_str, closed, p_tex) = case m_or_p_f of
                                              Left m_f ->
                                                  ( show m_f
                                                  , isProvableZ k m_f
                                                  , case proveZ k m_f of
                                                      [] -> ""
                                                      (p:_) -> tex p
                                                  )
                                              Right p_f ->
                                                  ( show p_f
                                                  , isProvableZ intui p_f
                                                  , case proveZ intui p_f of
                                                      [] -> ""
                                                      (p:_) -> tex p
                                                  )              in
              [ "<pre>Parsed input: " ++ f_str ++ "</pre>" -- TODO pretty? tex?
              , if closed
                  then "PROVED. <style type='text/css'> #output { border-color: green; } </style>\n"
                  else "NOT proved. <style type='text/css'> #output { border-color: red; } </style>\n"
              , if p_tex /= "" then "<div align='center'>\\( \\begin{prooftree}" ++ p_tex ++ " \\end{prooftree} \\)</div>" else ""
              ]

embeddedFile :: String -> T.Text
embeddedFile str = case str of
  "index.html" -> E.decodeUtf8 $(embedFile "exec/index.html")
  "jquery.js"  -> E.decodeUtf8 $(embedFile =<< runIO JQuery.file)
  _            -> error "File not found."
