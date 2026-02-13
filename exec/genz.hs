{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative

import General
import General.Lex
import General.Token

import FormP
import FormM
import FormM.Parse
import FormP.Parse
import FormP.ParseTPTP

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

data Input = FileInput FilePath | DirectInput String | StdInput

data InputFormat = InSingle | InTPTP

data Config = Config
  { input :: Input
  , inFormat :: InputFormat
  , tree :: Bool
  , negate :: Bool
  , debug :: Bool
  , proof :: ProofFormat
  , log :: MyLogic }

main :: IO ()
main = runGenZ =<< execParser opts where
  opts = info (configP <**> helper) (fullDesc <> progDesc "Prove the given FORMULA or the formula in FILE or STDIN."
    <> header "genz - a generic sequent calculus prover with zippers")

runGenZ :: Config -> IO ()
runGenZ (Config inp inf useTree negIn deb prFormat myL) = do
  form_in <- case inp of FileInput file  -> readFile file -- TODO: ignore "begin" and "end" here or in Lexer/Parser?
                         DirectInput f_s -> return f_s
                         StdInput        -> getContents -- TODO same

  let form_s = case inf of InSingle -> form_in
                           InTPTP -> case rewriteTPTPProblem form_in of Left _ -> error "Could not parse TPTP file"
                                                                        Right fs -> fs

  let tl = case myLex form_s of Left errs -> error $ unlines errs
                                Right tl_ -> tl_
  putStrLn $ case myL of
    Left l_prop ->
      case myParse form_s tl parseFormP of
        Left errs -> error (unlines errs)
        Right f_prop ->
          (if deb then "Tree size of formula: " ++ show (size $ neg f_prop) ++ "\n" else "") ++
          prChoose useTree prFormat l_prop ((if negIn then neg else id) f_prop)
    Right l_mod ->
      case myParse form_s tl parseFormM of
        Left errs -> error (unlines errs)
        Right f_mod ->
          (if deb then "Tree size of formula: " ++ show (size $ neg f_mod) ++ "\n" else "") ++
          prChoose useTree prFormat l_mod ((if negIn then neg else id) f_mod)

-- | Helper to chooe tree/zipper and bool/proof output
prChoose :: (Show f, Ord f, TeX f) => Bool -> ProofFormat -> Logic f -> (f -> String)
prChoose True  None  l = show . isProvableT l
prChoose True  Plain l = ppProof        . head . proofsT l
prChoose True  Buss  l = concatMap tex  . take 1 . proofsT l
prChoose True  Size  l = show . proofSize . head . proofsT l
prChoose False None  l = show . isProvableZ l
prChoose False Plain l = ppProof       . head . proofsZ l
prChoose False Buss  l = concatMap tex . take 1 . proofsZ l
prChoose False Size  l = show . proofSize . head . proofsZ l

configP :: Parser Config
configP = Config
      <$> inputP
      <*> option (maybeReader inputFormatR)
          ( long "input"
         <> short 'i'
         <> help "Input format: single, tptp"
         <> showDefaultWith ppInputFormat
         <> value InSingle
         <> metavar "INPUT" )
      <*> Options.Applicative.switch
          ( long "tree"
         <> short 't'
         <> help "Use standard trees (default is to use zippers)." )
      <*> Options.Applicative.switch
          ( long "negate"
         <> short 'n'
         <> help "Negate the input formula." )
      <*> Options.Applicative.switch
          ( long "debug"
         <> short 'd'
         <> help "Print additional debug information." )
      <*> option (maybeReader outputR)
          ( long "proofFormat"
         <> short 'p'
         <> help "Proof format: none, plain, buss, size"
         <> showDefaultWith ppProofFormat
         <> value None
         <> metavar "FORMAT" )
      <*> option (maybeReader logicR)
          ( long "logic"
         <> short 'l'
         <> help "Logic to use: CPL, IPL, D, D4, D45, GL, K, K4, K45, S4, T"
         <> showDefaultWith (\ case Left l -> name l; Right l -> name l)
         <> value (Right K.k)
         <> metavar "LOGIC" )

ppInputFormat :: InputFormat -> String
ppInputFormat InSingle = "single"
ppInputFormat InTPTP  = "tptp"

inputFormatR :: String -> Maybe InputFormat
inputFormatR i_s = case i_s of
  "single" -> return InSingle
  "tptp" -> return InTPTP
  "TPTP" -> return InTPTP
  _ -> error $ "Unknown input format: " ++ i_s

data ProofFormat = None | Plain | Buss | Size

ppProofFormat :: ProofFormat -> String
ppProofFormat None  = "none"
ppProofFormat Plain = "plain"
ppProofFormat Buss  = "buss"
ppProofFormat Size  = "size"

outputR :: String -> Maybe ProofFormat
outputR o_s = case o_s of
  "none" -> return None
  "plain" -> return Plain
  "buss" -> return Buss
  "size" -> return Size
  _ -> error $ "Unknown output format: " ++ o_s

type MyLogic = (Either (Logic FormP) (Logic FormM))

logicR :: String -> Maybe MyLogic
logicR l_s = case l_s of
  "CPL" -> return $ Left CPL.classical
  "IPL" -> return $ Left IPL.intui
  "D"   -> return $ Right D.d
  "D4"  -> return $ Right D4.dfour
  "D45" -> return $ Right D45.dfourfive
  "GL"  -> return $ Right GL.gl
  "K"   -> return $ Right K.k
  "K4"  -> return $ Right K4.kfour
  "K45" -> return $ Right K45.kfourfive
  "S4"  -> return $ Right S4.sfour
  "T"   -> return $ Right T.t
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
