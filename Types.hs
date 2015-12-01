module Types
       ( Clause
       , Env
       , Interp ) where

type Clause = [Int]
type Env = [(Int, Bool)]
type Interp = Bool

