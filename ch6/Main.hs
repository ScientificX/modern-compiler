module Main where

import Parser (parse)
import Lexer (alexScanTokens)
import Semant (transProg)
import System.Environment (getArgs)
import FindEscape (findEscape)

tokenizeFile :: String -> IO ()
tokenizeFile file = do
  program <- readFile file
  print $ alexScanTokens program

parseFile :: String -> IO ()
parseFile file = do
  program <- readFile file
  print $ parse $ alexScanTokens program

findEscapeFile :: String -> IO ()
findEscapeFile file = do
  program <- readFile file
  let (e, escaped) = findEscape $ parse $ alexScanTokens program in
    do
      putStrLn $ show escaped
      putStrLn "\n"
      putStrLn $ show e

transProgFile :: String -> IO ()
transProgFile file = do
  program <- readFile file
  let (_, escapedAst) = findEscape $ parse $ alexScanTokens program in
    transProg $ escapedAst

main :: IO ()
main = do
  args <- getArgs
  mapM_ transProgFile args
