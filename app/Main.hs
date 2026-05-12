module Main where

import Lexer
import Parser
import TypeCheck

main :: IO ()
main = do
  source <- readFile "app/source.my"
  let tokens = lexer source
  print tokens
  let programUnchecked = parse tokens
  print programUnchecked
  -- let programChecked = typeCheck programUnchecked
  -- print programChecked