module SAT where

import Parser

import System.Environment

main = do
  args <- getArgs
  let file = head args
  parseFile file
