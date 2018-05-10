module FileRunner where

import Parser (parse)
import Lexer (alexScanTokens)

parseFile :: String -> IO ()
parseFile file = do
  program <- readFile file
  let tokens = alexScanTokens program
  -- print tokens
  print $ parse tokens
