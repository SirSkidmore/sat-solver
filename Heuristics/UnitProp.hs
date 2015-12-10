module Heuristics.UnitProp
       ( unitPropogate
       , simplify
       ) where

import Types

import Data.Maybe

unitPropogate :: SolverState -> SolverState
unitPropogate (SolverState f r) =
  case getUnit f of Nothing -> SolverState f r
                    Just u -> unitPropogate $ SolverState (simplify f u) (u:r)

getUnit :: Formula -> Maybe Literal
getUnit xs = listToMaybe [x | [x] <- xs]

simplify :: Formula -> Literal -> Formula
simplify f l = [ simpClause x l | x <- f, not (elem l x) ]
  where simpClause c l = filter (\x -> x /= -l) c
