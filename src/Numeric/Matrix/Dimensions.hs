{-# LANGUAGE Haskell2010
    , TypeFamilies
    , FlexibleInstances
    , UndecidableInstances
 #-}

module Numeric.Matrix.Dimensions where

import qualified Numeric.Matrix as M

data Z
data S x

newtype Matrix n m e = Matrix (M.Matrix e) 


class AnyMatrix m where
    type MatrixElement m

    isEmpty :: m -> Bool

instance AnyMatrix (Matrix Z Z e) where
    type MatrixElement (Matrix Z Z e) = e

    isEmpty _ = True

instance AnyMatrix (Matrix Z (S n) e) where
    type MatrixElement (Matrix Z (S n) e) = e

    isEmpty _ = True

instance AnyMatrix (Matrix (S m) Z e) where
    type MatrixElement (Matrix (S m) Z e) = e

    isEmpty _ = True

instance AnyMatrix (Matrix (S m) (S n) e) where
    type MatrixElement (Matrix (S m) (S n) e) = e

    isEmpty _ = False


class AnyMatrix m => SquareMatrix m where
    inv :: m -> m
    det :: m -> MatrixElement m

instance SquareMatrix (Matrix (S n) (S n) e) where
    inv (Matrix m) = maybe undefined id (M.inv m)
    det = undefined


