module Brute
       ( bruteSolve
       ) where

import Types
import Parser
import Utils

genTestEnvs :: Env -> Int -> [Env]
genTestEnvs init 0 = [[]]
genTestEnvs init x =
  if not (lookupEnv init x)
  then [(x, True):g | g <- gs] ++ [(x, False):g | g <- gs]
  else genTestEnvs init (x - 1)
  where gs = genTestEnvs init (x - 1)


cleanClauses :: Env -> [Clause] -> [Clause]
cleanClauses init clauses =
  filter (\x -> not $ cleanLine init x) clauses

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

