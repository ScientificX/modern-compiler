module Tokenizer where

import Lexer

tokenizeFile :: String -> IO ()
tokenizeFile file = do
  program <- readFile file
  print $ alexScanTokens program
