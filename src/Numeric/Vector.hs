{-# LANGUAGE Haskell2010
    , TypeFamilies
    , FlexibleContexts
    , Trustworthy
    , StandaloneDeriving
    , DeriveDataTypeable
 #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module Numeric.Vector (
    
    Vector

) where

import Data.Ratio
import Data.Complex
import Data.Int

import Data.Array.IArray
import Data.Array.Unboxed

import qualified Data.Array.Unsafe as U

import Data.Typeable

data family Vector e

deriving instance Typeable Vector

data instance Vector Int
    = IntVector !Int (UArray Int Int)

data instance Vector Float
    = FloatVector !Int (UArray Int Float)

data instance Vector Double
    = DoubleVector !Int (UArray Int Double)

data instance Vector Integer
    = IntegerVector !Int (Array Int Integer)

data instance Vector (Ratio a)
    = RatioVector !Int (Array Int (Ratio a))

data instance Vector (Complex a)
    = ComplexVector !Int (Array Int (Complex a))

-- <.>
dotProd = undefined

-- ><
vectorProd = undefined


