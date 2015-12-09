module Brute
       ( bruteSolve
       ) where

import Parser
import Types
import Utils

genTestEnvs :: Env -> Int -> [Env]
genTestEnvs init 0 = [[]]
genTestEnvs init x =
  if not (isInEnv x init)
  then [(x, True):g | g <- gs] ++ [(x, False):g | g <- gs]
  else genTestEnvs init (x - 1)
  where gs = genTestEnvs init (x - 1)

checkEnv :: Env -> [Clause] -> Bool
checkEnv env [] = True
checkEnv env (x:xs) =
  cleanLine env x && checkEnv env xs

checkEnvs :: [Env] -> [Clause] -> Env
checkEnvs [] x = []
checkEnvs (x:xs) clauses =
  if checkEnv x clauses
  then x
  else checkEnvs xs clauses

bruteSolve :: Env -> [Clause] -> Int -> Env
bruteSolve init clauses n =
  let envs = [init ++ x | x <- genTestEnvs init n]
  in checkEnvs envs clauses

