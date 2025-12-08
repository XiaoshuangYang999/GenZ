{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative

import General
import General.Lex
import General.Token

import FormM
import FormM.Parse
import FormP
import FormP.Parse

import Logic.CPL
import Logic.IPL
import Logic.D
import Logic.D4
import Logic.GL
import Logic.K
import Logic.K4
import Logic.S4
import Logic.T

data Input = FileInput FilePath | DirectInput String | StdInput

data Config = Config
  { input :: Input
  , tree :: Bool
  , proof :: Bool
  , log :: MyLogic }

main :: IO ()
main = runGenZ =<< execParser opts where
  opts = info (configP <**> helper) (fullDesc <> progDesc "Prove the given FORMULA or the formula in FILE or STDIN."
    <> header "genz - a generic sequent calculus prover with zippers")

runGenZ :: Config -> IO ()
runGenZ (Config inp useTree showProof myL) = do
  form_s <- case inp of FileInput file  -> readFile file -- TODO: ignore "begin" and "end" here or in Lexer/Parser?
                        DirectInput f_s -> return f_s
                        StdInput        -> getContents -- TODO same
  let tl = case myLex form_s of Left errs -> error $ unlines errs
                                Right tl_ -> tl_
  putStrLn $ case myL of
    Left l_prop ->
      case myParse form_s tl parseFormP of
        Left errs -> error (unlines errs)
        Right f_prop -> prChoose useTree showProof l_prop f_prop
    Right l_mod ->
      case myParse form_s tl parseFormM of
        Left errs -> error (unlines errs)
        Right f_mod -> prChoose useTree showProof l_mod f_mod

-- | Helper to chooe tree/zipper and bool/proof output
prChoose :: (Show f, Ord f) => Bool -> Bool -> Logic f -> (f -> String)
prChoose True  True  l = show . proveT l
prChoose True  False l = show . isProvableT l
prChoose False True  l = show . proveZ l
prChoose False False l = show . isProvableZ l

configP :: Parser Config
configP = Config
      <$> inputP
      <*> Options.Applicative.switch
          ( long "tree"
         <> short 't'
         <> help "Use standard trees (default is to use zippers)." )
      <*> Options.Applicative.switch
          ( long "proof"
         <> short 'p'
         <> help "Print the (partial) proof (default is only True/False)." )
      <*> option (maybeReader logicR)
          ( long "logic"
         <> short 'l'
         <> help "Logic to use: CPL, IPL, D, D4, GL, K, K4, S4, T"
         <> showDefaultWith (\ case Left l -> name l; Right l -> name l)
         <> value (Right Logic.K.k)
         <> metavar "LOGIC" )

type MyLogic = (Either (Logic FormP) (Logic FormM))

logicR :: String -> Maybe MyLogic
logicR l_s = case l_s of
  "CPL" -> return $ Left Logic.CPL.classical
  "IPL" -> return $ Left Logic.IPL.intui
  "D"   -> return $ Right Logic.D.d
  "D4"  -> return $ Right Logic.D4.dfour
  "GL"  -> return $ Right Logic.GL.gl
  "K"   -> return $ Right Logic.K.k
  "K4"  -> return $ Right Logic.K4.kfour
  "S4"  -> return $ Right Logic.S4.sfour
  "T"   -> return $ Right Logic.T.t
  _ -> error $ "Unknown logic: " ++ l_s

fileInputP :: Parser Input
fileInputP = FileInput <$> strOption (long "file" <> short 'f' <> metavar "FILE" <> help "Input file" )

directInputP :: Parser Input
directInputP = DirectInput <$> strOption (long "formula" <> short 'F' <> metavar "FORMULA" <> help "Formula" )

stdInputP :: Parser Input
stdInputP = flag' StdInput (long "stdin" <> short 's' <> help "Read from stdin" )

inputP :: Parser Input
inputP = directInputP <|> fileInputP <|> stdInputP

myLex :: String -> Either [String] [Token AlexPosn]
myLex textinput = do
  let lexResult = alexScanTokensSafe textinput
  case lexResult of
    Left (line,col) -> Left
      [ "\nINPUT: " ++ textinput
      , replicate (col + length ("INPUT:" :: String)) ' ' ++ "^"
      , "Lexing error in line " ++ show line ++ " column " ++ show col ++ "." ]
    Right tokenList -> Right tokenList

myParse :: String -> [Token AlexPosn] -> ([Token AlexPosn] -> Either (Int,Int) f) -> Either [String] f
myParse textinput tl myParser = case myParser tl of
  Left (line,col) -> Left
    [ "\nINPUT: " ++ textinput
    , replicate (col + length ("INPUT:" :: String)) ' ' ++ "^"
    , "Parse error in line " ++ show line ++ " column " ++ show col ]
  Right frm -> Right frm
