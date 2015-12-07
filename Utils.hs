module Utils
       ( checkInterp
       , cleanLine
       , lookupEnv
       ) where

import Parser
import Types

import System.IO

lookupEnv :: Env -> Int -> Interp
lookupEnv [] n = False
lookupEnv (x:xs) n =
  if fst x == n
  then snd x
  else lookupEnv xs n

cleanLine :: Env -> Clause -> Bool
cleanLine env clause =
  foldr (||) False (map (\x -> if x < 0
                               then not $ lookupEnv env (abs x)
                               else lookupEnv env x) clause)

checkInterp :: Env -> [Clause] -> Bool
checkInterp env [] = True
checkInterp env (x:xs) =
  cleanLine env x && checkInterp env xs


  
