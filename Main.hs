module Main where

import Brute
import DPLL
import Parser
import Types
import Utils

import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  let mood = head args
  contents <- readFile $ args !! 1
  -- parseFile :: String -> ((Int, Env), [Clause])
  let parsed = parseFile contents
  -- ((Int, Env), [Clause]) -> Int
  let num = fst $ fst parsed
  -- ((Int, Env), [Clause]) -> Env
  let env = snd $ fst parsed
  -- -- ((Int, Env), [Clause]) -> [Clause]
  let cls = snd parsed
  let result = case mood of "brute" -> show $ bruteSolve env cls num
                            "check" -> let testEnv = read (args !! 2) :: Env
                                       in show $ checkInterp testEnv cls
                            "dpll" -> show $ solve cls
  putStrLn result
