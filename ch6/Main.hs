module Main where

import Parser (parse)
import Lexer (alexScanTokens)
import Semant (transProg)
import System.Environment (getArgs)

tokenizeFile :: String -> IO ()
tokenizeFile file = do
  program <- readFile file
  print $ alexScanTokens program

parseFile :: String -> IO ()
parseFile file = do
  program <- readFile file
  print $ parse $ alexScanTokens program

transProgFile :: String -> IO ()
transProgFile file = do
  program <- readFile file
  let tokens = alexScanTokens program
  transProg $ parse tokens

main :: IO ()
main = do
  args <- getArgs
  mapM_ transProgFile args
