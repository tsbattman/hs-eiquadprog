
module Numeric.Optimization.QuadProg.Binding (
    c'hs_solve_quadprog
  , c'hs_is_solution
  ) where

import Foreign
import Foreign.C

#include <eiquadprog.h>

-- http://www.labri.fr/perso/guenneba/code/QuadProg/eiquadprog.hpp
foreign import ccall "hs_is_solution"
  c'hs_is_solution :: CDouble -> CChar


{-|
 NOTE: this is a modified of uQuadProg++ package, working with Eigen data structures.
       uQuadProg++ is itself a port made by Angelo Furfaro of QuadProg++ originally developed by
       Luca Di Gaspero, working with ublas data structures.

 The quadprog_solve() function implements the algorithm of Goldfarb and Idnani
 for the solution of a (convex) Quadratic Programming problem
by means of a dual method.

The problem is in the form:

min 0.5 * x G x + g0 x
s.t.
    CE^T x + ce0 = 0
    CI^T x + ci0 >= 0

 The matrix and vectors dimensions are as follows:
    G: n * n
    g0: n

    CE: n * p
    ce0: p

    CI: n * m
    ci0: m

    x: n

 The function will return the cost of the solution written in the x vector or
 std::numeric_limits::infinity() if the problem is infeasible. In the latter case
 the value of the x vector is not correct.

 References: D. Goldfarb, A. Idnani. A numerically stable dual method for solving
             strictly convex quadratic programs. Mathematical Programming 27 (1983) pp. 1-33.

 Notes:
  1. pay attention in setting up the vectors ce0 and ci0.
     If the constraints of your problem are specified in the form
     A^T x = b and C^T x >= d, then you should set ce0 = -b and ci0 = -d.
  2. The matrix G is modified within the function since it is used to compute
     the G = L^T L cholesky factorization for further computations inside the function.
     If you need the original matrix G you should make a copy of it and pass the copy
     to the function.


 The author will be grateful if the researchers using this software will
 acknowledge the contribution of this modified function and of Di Gaspero's
 original version in their research papers.


LICENSE



This file is a porting of QuadProg++ routine, originally developed
by Luca Di Gaspero, exploiting uBlas data structures for vectors and
matrices instead of native C++ array.
-}
foreign import ccall "hs_solve_quadprog"
  c'hs_solve_quadprog ::
       Ptr CDouble -- G
    -> Ptr CDouble -- g0
    -> Ptr CDouble -- CE
    -> Ptr CDouble -- ce0
    -> Ptr CDouble -- CI
    -> Ptr CDouble -- ci0
    -> Ptr CDouble -- x
    -> CInt -- n
    -> CInt -- p
    -> CInt -- m
    -> IO CDouble
