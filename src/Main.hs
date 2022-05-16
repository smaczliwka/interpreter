module Main where

import System.Environment (getArgs)
import System.Exit        (exitFailure)

import AbsMyLatte   (Program)
import LexMyLatte   (Token)
import ParMyLatte   (pProgram, myLexer)
import Typechecker (typecheck)
import Interpreter (interpret)

type Err        = Either String
type ParseFun a = [Token] -> Err a

runFile :: ParseFun Program -> FilePath -> IO ()
runFile p f = putStrLn f >> readFile f >>= run p

run :: ParseFun Program  -> String -> IO ()
run p s =
  case p ts of
    Left error -> do
      putStrLn error
    Right prog -> do
      case typecheck prog of
        Just error -> do
            putStrLn error
        Nothing -> interpret prog
  where
  ts = myLexer s

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  (files)         Parse content of files verbosely."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run pProgram
    fs         -> mapM_ (runFile pProgram) fs
