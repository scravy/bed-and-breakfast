bed-and-breakfast
=================

Matrix operations in 100% pure Haskell.

Bed and Breakfast is a linear algebra library written in Haskell.
It provides fast matrix operations like finding the determinant
or the inverse of a matrix.

- [API documentation](https://hackage.haskell.org/package/bed-and-breakfast-0.4.1/docs/Numeric-Matrix.html)
- bed-and-breakfast on [Hackage](https://hackage.haskell.org/package/bed-and-breakfast)

Example (GHCi Session)
-----------------------

    *Numeric.Matrix> let m = fromList [[0,3,2],[5,6,10],[4,3,2.0]] :: Matrix Double
    *Numeric.Matrix> inv m
    Just  -0.2499999999999999  0.0  0.25
    0.4166666666666667  -0.11111111111111112  0.1388888888888889
    -0.12500000000000006  0.16666666666666669  -0.20833333333333334

    *Numeric.Matrix> let m = fromList [[0,3,2],[5,6,10],[4,3,2.0]] :: Matrix Rational
    *Numeric.Matrix> inv m
    Just  (-1) % 4  0 % 1  1 % 4
    5 % 12  (-1) % 9  5 % 36
    (-1) % 8  1 % 6  (-5) % 24
    
Example (with Template Haskell Syntactic Sugar)
------------------------------------------------

    {-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}
    
    import Numeric.Matrix
    import Numeric.Matrix.Sugar
    
    m :: Matrix Double
    m = [dMatrix| 20   30 40
                  40.5 71 23
                  20   20 27 |]

    mInv = maybe (error "not invertible") id $ inv m

