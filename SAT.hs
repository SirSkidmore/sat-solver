module SAT where

import Brute
import Parser

import System.Environment

main = do
  args <- getArgs
  let mood = head args
  let file = parseFile (args !! 1)
  -- let envs = fst file
  -- let cls = snd file
  -- let result = case mood of "brute" -> bruteSolve envs cls
  return $ fst file

