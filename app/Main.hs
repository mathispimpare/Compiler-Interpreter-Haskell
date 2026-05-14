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
  let programChecked = typeCheck programUnchecked
  case programChecked of
    Right program -> print ("Types are correct, program checked : " ++ show program)
    Left err -> putStrLn("Type error : " ++ err)