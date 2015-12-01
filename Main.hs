module Main where

import Brute
import Parser

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let mood = head args
  contents <- readFile $ args !! 1
  let parsed = parseFile contents
  let envs = fst parsed
  let cls = snd parsed
  let result = case mood of "brute" -> bruteSolve envs cls
  -- dummy results, please ignore
  putStrLn $ show result

