module Utils
       ( checkInterp
       , cleanLine
       , lookupEnv
       , isInEnv
       ) where

import Data.Maybe
import Parser
import Types

import System.IO

checkEnv :: Env -> Int -> Maybe Interp
checkEnv [] n = Nothing
checkEnv (x:xs) n =
  if fst x == n
  then Just $ snd x
  else checkEnv xs n

isInEnv :: Int -> Env -> Bool
isInEnv n env =
  isJust $ checkEnv env (abs n)

lookupEnv :: Env -> Int -> Interp
lookupEnv env n =
  if n < 0
  then (not . fromJust) $ checkEnv env (abs n)
  else fromJust $ checkEnv env n

-- must be complete environment to clean
cleanLine :: Env -> Clause -> Bool
cleanLine env clause =
  foldr (||) False (map (lookupEnv env) clause)

checkInterp :: Env -> [Clause] -> Bool
checkInterp env [] = True
checkInterp env (x:xs) =
  cleanLine env x && checkInterp env xs


  
