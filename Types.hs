module Types
       ( Clause , Env
       , Interp , Formula
       , Record , Literal
       , SolverState(..) ) where

data SolverState =
  SolverState { formula :: Formula
              , record :: Record
              } deriving (Show)

type Literal = Int
type Clause = [Literal]
type Formula = [Clause]
type Record = [Literal]
type Env = [(Int, Bool)]
type Interp = Bool
