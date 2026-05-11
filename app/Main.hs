module Main where

import Parser
import Lexer

main :: IO ()
main = do
  source <- readFile "app/source.my"
  let tokens = lexer source
  let program = parse tokens
  print program