module Brute
       ( bruteSolve
       ) where

import Types

bruteSolve :: [Env] -> [Clause] -> Env
bruteSolve (env:envs) (x:xs) = env
