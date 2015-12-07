module Parser
       ( parseFile
       ) where

import Types

numVars :: [String] -> Int
numVars [] = -1
numVars (x:xs) =
  let wrds = words x
  in if (head wrds) == "p"
     then read (wrds !! 2) :: Int
     else numVars xs

inEnv :: Int -> Env -> Bool
inEnv n [] = False
inEnv n (x:xs) =
  if (fst x) == n
  then True
  else inEnv n xs

parseWords :: [String] -> Clause
parseWords [] = []
parseWords (s:ss) =
  let n = read s :: Int
  in if n /= 0
     then n:parseWords ss
     else parseWords ss

parseLines :: [String] -> [Clause]
parseLines [] = []
parseLines (s:ss) =
  if (head s) == 'c' || (head s) == 'p'
  then parseLines ss
  else (parseWords (words s)):parseLines ss

genInitialEnv :: [Clause] -> Env
genInitialEnv [] = []
genInitialEnv (x:xs) =
  if (length x) == 1
  then if (head x) > 0
       then ((head x), True):genInitialEnv xs
       else (abs(head x), False):genInitialEnv xs
  else genInitialEnv xs

parseFile :: String -> ((Int, Env), [Clause])
parseFile s =
  let lins = lines s
      parsed = parseLines $ lins
  in (((numVars lins), (genInitialEnv parsed)), parsed)
              
