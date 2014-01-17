{-# LANGUAGE Haskell2010
    , TypeFamilies
    , FlexibleContexts
    , Trustworthy
 #-}
{-# OPTIONS -Wall -fno-warn-name-shadowing #-}

module Numeric.Vector (
    
    Vector

) where


data family Vector e


data instance Matrix Int
    = IntMatrix !Int (UArray Int Int)

data instance Matrix Float
    = FloatMatrix !Int (UArray Int Float)

data instance Matrix Double
    = DoubleMatrix !Int (UArray Int Double)

data instance Matrix Integer
    = IntegerMatrix !Int (Array Int Integer)

data instance Matrix (Ratio a)
    = RatioMatrix !Int (Array Int (Ratio a))

data instance Matrix (Complex a)
    = ComplexMatrix !Int (Array Int (Complex a))


instance Typeable a => Typeable (Vector a) where
    typeOf x = mkTyConApp (mkTyCon3 "bed-and-breakfast"
                                    "Numeric.Vector"
                                    "Vector") [typeOf (unT x)]
      where
        unT :: Matrix a -> a
        unT = undefined



-- <.>
dotProd = undefined

-- ><
vectorProd = undefined


