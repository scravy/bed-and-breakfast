{-# LANGUAGE Haskell2010, TemplateHaskell #-}

import Control.Monad

import Numeric.Matrix

import Test.QuickCheck
import Test.QuickCheck.All

import System.Exit


dim :: Num a => a
dim = 6

instance (MatrixElement e, Arbitrary e) => Arbitrary (Matrix e) where

    arbitrary = sequence (replicate dim (vector dim)) >>= return . fromList


prop_plus_commutative :: Matrix Double -> Matrix Double -> Bool
prop_plus_commutative m1 m2 = m1 + m2 == m2 + m1

prop_zero_det :: Matrix Rational -> Bool
prop_zero_det m1 = let m1' = inv m1 in case m1' of (Just _) -> det m1 /= 0; _ -> det m1 == 0

prop_inv :: Matrix Rational -> Bool
prop_inv m1 = let m1' = inv m1 in case m1' of (Just m1') -> m1' * m1 == unit dim; _ -> True

prop_rank :: Matrix Rational -> Bool
prop_rank m1 = let r = rank m1 in if r < dim then det m1 == 0 else r == dim

prop_trace_select :: Matrix Rational -> Bool
prop_trace_select m = trace m == select (uncurry (==)) m

prop_transpose_twice :: Matrix Rational -> Bool
prop_transpose_twice m = transpose (transpose m) == m

prop_inv_twice :: Matrix Rational -> Bool
prop_inv_twice m1 = let m1' = inv m1 in case m1' of (Just m1') -> inv m1' == Just m1; _ -> True


main = do

    success <- $(quickCheckAll)
    
    (if success then exitSuccess else exitFailure)


