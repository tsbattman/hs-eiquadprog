
module Numeric.Optimization.QuadProg.Solve (
    Solution(..)
  , solveQuadProg'
  , solveQuadProg
  ) where

import Foreign
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.Vector.Storable as VS

import Numeric.Optimization.QuadProg.Binding

isSolution :: Double -> Bool
isSolution = toBool . c'hs_is_solution . realToFrac

data Solution = Infeasible
  | Solution !Double !(VS.Vector Double)
  deriving (Eq, Show, Read)

{-|
 - Matrices are in column-major format and are dense, no strides allowed for now
 -}
solveQuadProg' :: VS.Vector Double -- G
    -> VS.Vector Double -- g0
    -> VS.Vector Double -- CE
    -> VS.Vector Double -- ce0
    -> VS.Vector Double -- CI
    -> VS.Vector Double -- ci0
    -> IO Solution
solveQuadProg' g g0 ce ce0 ci ci0 =
  VS.unsafeWith (VS.unsafeCast g)   $ \pg   ->
  VS.unsafeWith (VS.unsafeCast g0)  $ \pg0  ->
  VS.unsafeWith (VS.unsafeCast ce)  $ \pce  ->
  VS.unsafeWith (VS.unsafeCast ce0) $ \pce0 ->
  VS.unsafeWith (VS.unsafeCast ci)  $ \pci  ->
  VS.unsafeWith (VS.unsafeCast ci0) $ \pci0 -> do
    x <- mallocForeignPtrArray n
    v <- withForeignPtr x $ \px ->
      realToFrac <$> c'hs_solve_quadprog pg pg0 pce pce0 pci pci0 px (fromIntegral n) (fromIntegral p) (fromIntegral m)
    if not $ isSolution v
      then return Infeasible
      else return $! Solution v (VS.unsafeCast (VS.unsafeFromForeignPtr0 x n))
  where
    n = VS.length g0
    p = VS.length ce0
    m = VS.length ci0

solveQuadProg :: VS.Vector Double -- G
    -> VS.Vector Double -- g0
    -> VS.Vector Double -- CE
    -> VS.Vector Double -- ce0
    -> VS.Vector Double -- CI
    -> VS.Vector Double -- ci0
    -> Solution
solveQuadProg g g0 ce ce0 ci ci0 = unsafePerformIO $ solveQuadProg' g g0 ce ce0 ci ci0
