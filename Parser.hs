module Parser
       ( parseFile
       ) where

import System.IO

type Clause = [Int]
type Env = [(Int, Bool)]

numVars :: [String] -> Int
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

genTestEnvs :: Env -> Int -> [Env]
genTestEnvs init 0 = [[]]
genTestEnvs init x =
  if not (inEnv x init)
  then [(x, True):g | g <- gs] ++ [(x, False):g | g <- gs]
  else genTestEnvs init (x - 1)
  where gs = genTestEnvs init (x - 1)

parseFile :: String -> IO ([Env], [Clause])
parseFile f = do
  contents <- readFile f
  let parsed = parseLines (lines contents)
  let testEnvs =
        [init ++ x | x <- genTestEnvs init (numVars (lines contents))]
        where init = genInitialEnv parsed
  return (testEnvs, parsed)
