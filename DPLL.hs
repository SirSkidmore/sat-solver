module DPLL
       ( solve
       ) where

import Heuristics.UnitProp
import Types

import Data.Maybe

dpll :: SolverState -> Maybe Record
dpll s
  | null f = return r
  | otherwise = do
      l <- chooseLiteral f
      case dpll (SolverState (simplify f l) (l:r)) of
        Just record -> return record
        Nothing -> dpll $ SolverState (simplify f (-l)) ((-l):r)
  where
    s' = unitPropogate s
    f = formula s'
    r = record s'

chooseLiteral :: Formula -> Maybe Literal
chooseLiteral f = listToMaybe . concat $ f

solve :: Formula -> Maybe [Literal]
solve f = dpll $ SolverState f []

