{-# LANGUAGE Haskell2010
    , TypeFamilies
    , FlexibleContexts
    , Trustworthy
    , StandaloneDeriving
    , DeriveDataTypeable
    , CPP
 #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}
{-# OPTIONS -cpp  -pgmPcpphs  -optP--cpp #-}

module Numeric.Vector (
    
    Vector

) where

import Data.Ratio
import Data.Complex
import Data.Int

import Data.Array.IArray
import Data.Array.Unboxed

import Data.Typeable

import qualified Data.Array.Unsafe as U

data family Vector e

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
deriving instance Typeable Vector
#else
deriving instance Typeable1 Vector
#endif


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


