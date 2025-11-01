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
import FormM.Lex
import FormM.Parse
-- TODO import multiple logics, qualified, add command line arg to choose it
import K

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
      -- logic <- param "logic"
      textinput <- param "textinput"
      let parseResult = parseFormM (alexScanTokens textinput) -- FIXME use safe alexScan
      html $ mconcat $ map TL.pack $ case parseResult of
        Left (_,col) ->
          [ "<pre>INPUT: " ++ textinput ++ "</pre>"
          , "<pre>" ++ replicate (col + length ("INPUT:" :: String)) ' ' ++ "^</pre>"
          , "<pre>Parse error in column " ++ show col ++ ".</pre>" ]
        Right f ->
          let prfs = proveZ k f
              closed = isProvableZ k f
          in
          [ "<pre>Parsed input: " ++ show f ++ "</pre>" -- TODO pretty? tex?
          , if closed
              then "PROVED. <style type='text/css'> #output { border-color: green; } </style>\n"
              else "NOT proved. <style type='text/css'> #output { border-color: red; } </style>\n"
          , case prfs of
              [] -> ""
              (p:_) -> "<div align='center'>\\( \\begin{prooftree}" ++ tex p ++ " \\end{prooftree} \\)</div>"
          ]

embeddedFile :: String -> T.Text
embeddedFile str = case str of
  "index.html" -> E.decodeUtf8 $(embedFile "exec/index.html")
  "jquery.js"  -> E.decodeUtf8 $(embedFile =<< runIO JQuery.file)
  _            -> error "File not found."
