
module Main where

import Numeric.Optimization.QuadProg.Solve

import qualified Data.Vector.Storable as VS

main :: IO ()
main = do
  x <- solveQuadProg' g g0 ce ce0 ci ci0
  print g
  print x
  where
    g   = VS.fromList [1, 0.5, 0, 1]
    g0  = VS.fromList [2, 1]
    ce  = VS.fromList [1, 1]
    ce0 = VS.map negate $ VS.fromList [1]
    ci  = VS.empty
    ci0 = VS.empty

