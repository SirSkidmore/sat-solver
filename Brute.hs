module Brute
       ( bruteSolve
       ) where

import Types

lookupEnv :: Env -> Int -> Interp
lookupEnv [] n = False -- we should never hit this
lookupEnv (x:xs) n =
  if fst x == n
  then snd x
  else lookupEnv xs n

cleanLine :: Env -> Clause -> Bool
cleanLine env clause =
  foldr (||) False (map (\x -> if x < 0
                               then not $ lookupEnv env (abs x)
                               else lookupEnv env x) clause)

checkEnv :: Env -> [Clause] -> Bool
checkEnv env [] = True
checkEnv env (x:xs) =
  cleanLine env x && checkEnv env xs

bruteSolve :: [Env] -> [Clause] -> Env
bruteSolve [] x = []
bruteSolve (x:xs) clauses =
  if checkEnv x clauses
  then x
  else bruteSolve xs clauses
