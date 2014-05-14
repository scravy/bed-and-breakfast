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

-- | Efficient matrix operations in 100% pure Haskell.
--
-- This package uses miscellaneous implementations,
-- depending on the type of its components. Typically unboxed
-- arrays will perform best, while unboxed arrays give you
-- certain features such as 'Rational' or 'Complex' components.
--
-- The following component types are supported by 'Matrix':
-- 
-- [@Int@] Uses unboxed arrays internally. 'inv' will always
--      return 'Nothing'.
--
-- [@Integer@] Uses boxed arrays internally. 'inv' will always
--      return 'Nothing'.
--
-- [@Double@ and @Float@] Uses unboxed arrays internally.
--      All matrix operations will work as expected.
--      @Matrix Double@ will probably yield the best peformance.
--
-- [@Rational@] Best choice if precision is what you aim for.
--      Uses boxed arrays internally. All matrix operations will
--      work as expected.
--
-- [@Complex@] Experimental. Uses boxed arrays internally.
--      All matrix operations will work as expected, though
--      finding the inverse of a matrix isa tad less numerically
--      stable than with a @Double@ matrix.
module Numeric.Matrix (

    Matrix,

    MatrixElement (..),

    -- * Matrix property and utility functions.
    (<|>),
    (<->),
    scale,

    -- ** Matrix properties
    isUnit,
    isZero,
    isDiagonal,
    isEmpty,
    isSquare,

    -- ** Conversions
    toDoubleMatrix,
    toComplexMatrix,
    toRationalMatrix
    
) where


import Control.Applicative ((<$>))
import Control.DeepSeq
import Control.Monad
import Control.Monad.ST

import Data.Function (on)
import Data.Ratio
import Data.Complex
import Data.Maybe
import Data.Int
import Data.Word

import qualified Data.List as L
import Data.Array.IArray
import Data.Array.MArray
import Data.Array.Unboxed
import Data.Array.ST
import Data.STRef
import Data.Binary

import qualified Data.Array.Unsafe as U

import Data.Typeable

import Prelude hiding (any, all, read, map)
import qualified Prelude as P


-- | Matrices are represented by a type which fits best the component type.
-- For example a @Matrix Double@ is represented by unboxed arrays,
-- @Matrix Integer@ by boxed arrays.
--
-- Data instances exist for 'Int', 'Float', 'Double', 'Integer', 'Ratio',
-- and 'Complex'. Certain types do have certain disadvantages, like for
-- example you can not compute the inverse matrix of a @Matrix Int@.
--
-- Every matrix (regardless of the component type) has instances for
-- 'Show', 'Read', 'Num', 'Fractional', 'Eq', 'Typeable', 'Binary',
-- and 'NFData'. This means that you can use arithmetic operations like
-- '+', '*', and '/', as well as functions like 'show', 'read', or 'typeOf'.
--
-- [@Show (Matrix e)@]
-- Note that a Show instance for the component type @e@ must exist.
-- 
-- [@Read (Matrix e)@]
-- You can read a matrix like so:
--
-- > read "1 0\n0 1\n" :: Matrix Double
--
-- [@Num (Matrix e)@]
-- '+', '-', '*', 'negate', 'abs', 'signum', and 'fromInteger'.
--
-- 'signum' will compute the determinant and return the signum
-- of it.
--
-- 'abs' applies @map abs@ on the matrix (that is, it applies
-- @abs@ on every component in the matrix and returns a new
-- matrix without negative components).
--
-- @fromInteger@ yields a 1-x-1-matrix.
--
-- [@Fractional (Matrix e)@]
-- Only available if there exists an instance @Fractional e@
-- (the component type needs to have a @Fractional@ instance, too).
-- Note that while the 'Num' operations are safe, 'recip' and
-- '/' will fail (with an 'error') if the involved matrix is
-- not invertible or not a square matrix.
--
-- [@NFData (Matrix e)@]
-- Matrices have instances for NFData so that you can use a
-- matrix in parallel computations using the @Control.Monad.Par@
-- monad (see the @monad-par@ package for details).
--
-- [@Typeable (Matrix e)@]
-- Allows you to use matrices as 'Dynamic' values.
--
-- [@Binary (Matrix e)@]
-- Serialize and unserialize matrices using the @binary@ package.
-- See @encode@ and @decode@.
data family Matrix e

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 707)
deriving instance Typeable Matrix
#else
deriving instance Typeable1 Matrix
#endif

data instance Matrix Int
    = IntMatrix !Int !Int (Array Int (UArray Int Int))

data instance Matrix Float
    = FloatMatrix !Int !Int (Array Int (UArray Int Float))

data instance Matrix Double
    = DoubleMatrix !Int !Int (Array Int (UArray Int Double))

data instance Matrix Integer
    = IntegerMatrix !Int !Int (Array Int (Array Int Integer))

data instance Matrix (Ratio a)
    = RatioMatrix !Int !Int (Array Int (Array Int (Ratio a)))

data instance Matrix (Complex a)
    = ComplexMatrix !Int !Int (Array Int (Array Int (Complex a)))

instance (MatrixElement e, Show e) => Show (Matrix e) where
    show = unlines . P.map showRow . toList
      where
        showRow = unwords . P.map ((' ':) . show)

instance (Read e, MatrixElement e) => Read (Matrix e) where
    readsPrec _ = (\x -> [(x, "")]) . fromList . P.map (P.map P.read . words) . lines

instance (MatrixElement e) => Num (Matrix e) where
    (+) = plus
    (-) = minus
    (*) = times
    abs         = map abs
    signum      = matrix (1,1) . const . signum . det
    fromInteger = matrix (1,1) . const . fromInteger
    
instance (MatrixElement e, Fractional e) => Fractional (Matrix e) where
    recip        = fromJust . inv
    fromRational = matrix (1,1) . const . fromRational

instance (MatrixElement e) => Eq (Matrix e) where
    m == n
        | dimensions m == dimensions n
            = allWithIndex (\ix e -> m `at` ix == e) n
        | otherwise = False

instance (MatrixElement e) => NFData (Matrix e) where
    rnf matrix = matrix `deepseq` ()

instance (MatrixElement e, Binary e) => Binary (Matrix e) where
    
    put m = do
        let (rows, cols) = dimensions m
        
        put rows >> put cols

        forM_ [1..rows] $ \i -> do
            forM_ [1..cols] $ \j -> do
                put (m `at` (i,j))

    get = do
        rows <- get :: Get Int
        cols <- get :: Get Int

        forM [1..rows] (const (forM [1..cols] (const get)))
            >>= return . fromList


(<|>) :: MatrixElement e => Matrix e -> Matrix e -> Matrix e
-- ^ Joins two matrices horizontally.
--
-- > 1 2 3     1 0 0      1 2 3 1 0 0
-- > 3 4 5 <|> 2 1 0  ->  3 4 5 2 1 0
-- > 5 6 7     3 2 1      5 6 7 3 2 1
m1 <|> m2 = let m = numCols m1
                n1 = numRows m1
                n2 = numRows m2
            in matrix (max n1 n2, m + numCols m2)
              $ \(i,j) -> if j > m
                    then (if i > n2 then 0 else m2 `at` (i,j-m))
                    else (if i > n1 then 0 else m1 `at` (i,j))

(<->) :: MatrixElement e => Matrix e -> Matrix e -> Matrix e
-- ^ Joins two matrices vertically.
--
-- > 1 2 3     1 0 0      1 2 3
-- > 3 4 5 <-> 2 1 0  ->  3 4 5
-- > 5 6 7     3 2 1      5 6 7
-- >                      1 0 0
-- >                      2 1 0
-- >                      3 2 1
m1 <-> m2 = let m = numRows m1
                n1 = numCols m1
                n2 = numCols m2
            in matrix (m + numRows m2, max n1 n2)
              $ \(i,j) -> if i > m
                    then (if j > n2 then 0 else m2 `at` (i-m,j))
                    else (if j > n1 then 0 else m1 `at` (i,j))

scale :: MatrixElement e => Matrix e -> e -> Matrix e
-- ^ Scales a matrix by the given factor.
-- 
-- > scale s == map (*s)
scale m s = map (*s) m


isUnit, isDiagonal, isZero, isEmpty, isSquare :: MatrixElement e => Matrix e -> Bool

-- | Check whether the matrix consists of all zeros.
--
-- > isZero == all (== 0)
isZero = all (== 0)

-- | Check whether the matrix is an identity matrix.
--
-- > 1 0 0
-- > 0 1 0
-- > 0 0 1 (True)
isUnit m = isSquare m && allWithIndex (uncurry check) m
    where check = \i j e -> if i == j then e == 1 else e == 0

-- | Checks whether the matrix is empty.
--
-- > isEmpty m = numCols == 0 || numRows == 0
isEmpty m = numRows m == 0 || numCols m == 0

-- | Checks whether the matrix is a diagonal matrix.
--
-- > 4 0 0 0
-- > 0 7 0 0
-- > 0 0 3 0
-- > 0 0 0 9 (True)
isDiagonal m = isSquare m && allWithIndex (uncurry check) m
    where check = \i j e -> if i /= j then e == 0 else True

-- | Checks whether the matrix is a square matrix.
--
-- > isSquare == uncurry (==) . dimensions
isSquare m = let (a, b) = dimensions m in a == b


toDoubleMatrix :: (MatrixElement a, Integral a) => Matrix a -> Matrix Double
toDoubleMatrix = map fromIntegral

toRationalMatrix :: (MatrixElement a, Real a) => Matrix a -> Matrix Rational
toRationalMatrix = map toRational

toComplexMatrix :: (MatrixElement a, RealFloat a, Show a) => Matrix a -> Matrix (Complex a)
toComplexMatrix = map (:+ 0)


class Division e where
    divide :: e -> e -> e

instance Division Int     where divide = quot
instance Division Int8    where divide = quot
instance Division Int16   where divide = quot
instance Division Int32   where divide = quot
instance Division Int64   where divide = quot
instance Division Word8   where divide = quot
instance Division Word16  where divide = quot
instance Division Word32  where divide = quot
instance Division Word64  where divide = quot
instance Division Integer where divide = quot
instance Division Float   where divide = (/)
instance Division Double  where divide = (/)
instance Integral a => Division (Ratio a) where divide = (/)
instance RealFloat a => Division (Complex a) where divide = (/)


class (Eq e, Num e) => MatrixElement e where

    -- | Creates a matrix of the given size using a generator
    -- function for the value of each component.
    matrix :: (Int, Int) -> ((Int, Int) -> e) -> Matrix e

    -- | Builds a list from a matrix for the indices for which
    -- the given predicate matches.
    --
    -- > trace == select (uncurry (==))
    select :: ((Int, Int) -> Bool) -> Matrix e -> [e]

    -- | Returns the component at the given position in the matrix.
    -- Note that indices start at one, not at zero.
    at :: Matrix e -> (Int, Int) -> e

    -- | Returns the row at the given index in the matrix.
    -- Note that indices start at one, not at zero.
    row :: Int -> Matrix e -> [e]

    -- | Returns the row at the given index in the matrix.
    -- Note that indices start at one, not at zero.
    col :: Int -> Matrix e -> [e]

    -- | The dimensions of a given matrix.
    dimensions :: Matrix e -> (Int, Int)

    -- | The number of rows in the given matrix.
    numRows :: Matrix e -> Int

    -- | The number of columns in the given matrix.
    numCols :: Matrix e -> Int


    -- | Builds a matrix from a list of lists.
    --
    -- The innermost lists represent the rows. This function will create a m-n-matrix,
    -- where m is the number of rows, which is the minimum length of the row lists
    -- and n is the number of columns, i.e. the length of the outer list.
    --
    -- > fromList [[1,2,3],[2,1,3],[3,2,1]] :: Matrix Rational
    fromList :: [[e]] -> Matrix e

    -- | Turns a matrix into a list of lists.
    --
    -- > (toList . fromList) xs == xs
    --
    -- > (fromList . toList) mat == mat
    toList   :: Matrix e -> [[e]]

    -- | An identity square matrix of the given size.
    --
    -- >>> unit 4
    -- 1 0 0 0
    -- 0 1 0 0
    -- 0 0 1 0
    -- 0 0 0 1
    unit  :: Int -> Matrix e

    -- | A square matrix of the given size consisting of all zeros.
    -- 
    -- >>> zero 2
    -- 0 0
    -- 0 0
    zero  :: Int -> Matrix e

    -- | A square matrix which trace is the given list, all other components
    -- set to zero.
    --
    -- >>> diag [1,4,7,9]
    -- 1 0 0 0
    -- 0 4 0 0
    -- 0 0 7 0
    -- 0 0 0 9
    diag  :: [e] -> Matrix e

    -- | Check whether the matrix is the empty matrix.
    --
    -- > dimensions empty == (0, 0)
    empty :: Matrix e

    -- | Subtract a matrix from another.
    minus :: Matrix e -> Matrix e -> Matrix e

    -- | Add two matrices.
    --
    -- You may also use the 'Num' instance for matrices,
    -- i.e. @matrix1 + matrix2@ will work, too.
    plus  :: Matrix e -> Matrix e -> Matrix e

    -- | Multiply two matrices /O(n^3)/.
    --
    -- You may also use the 'Num' instance for matrices,
    -- i.e. @matrix1 * matrix2@ will work, too.
    times :: Matrix e -> Matrix e -> Matrix e

    -- | Compute the inverse of a matrix. Returns @Nothing@
    -- if the matrix is not invertible.
    inv   :: Matrix e -> Maybe (Matrix e)

    -- | Applies Bareiss multistep integer-preserving
    -- algorithm for finding the determinant of a matrix.
    -- Returns 0 if the matrix is not a square matrix.
    det       :: Matrix e -> e

    -- | Flip rows and columns.
    --
    -- > 1 8 9                1 2 3
    -- > 2 1 8  --transpose-> 8 1 2
    -- > 3 2 1                9 8 1 
    transpose :: Matrix e -> Matrix e

    -- | Compute the rank of a matrix.
    rank      :: Matrix e -> e

    -- | Select the diagonal elements of a matrix as a list.
    --
    -- > 1 8 3
    -- > 3 6 5 --trace-> [1, 6, 2]
    -- > 7 4 2
    trace     :: Matrix e -> [e]

    -- | Select the minor of a matrix, that is the determinant
    -- of the 'minorMatrix'.
    --
    -- > minor = det . minorMatrix
    minor :: MatrixElement e => (Int, Int) -> Matrix e -> e

    -- | Select the minor matrix of a matrix, a matrix that is obtained
    -- by deleting the i-th row and j-th column.
    --
    -- > 10  9 95 45
    -- >  8  7  3 27                        8  3 27
    -- > 13 17 19 23 --minorMatrix (1,2)-> 13 19 23
    -- >  1  2  5  8                        1  5  8
    minorMatrix :: MatrixElement e => (Int, Int) -> Matrix e -> Matrix e

    cofactors :: MatrixElement e => Matrix e -> Matrix e
    adjugate :: MatrixElement e => Matrix e -> Matrix e

    -- | Apply a function on every component in the matrix.
    map :: MatrixElement f => (e -> f) -> Matrix e -> Matrix f

    -- | Apply a predicate on every component in the matrix
    -- and returns True iff all components satisfy it.
    all :: (e -> Bool) -> Matrix e -> Bool

    -- | Apply a predicate on every component in the matrix
    -- and return True if one or more components satisfy it.
    any :: (e -> Bool) -> Matrix e -> Bool

    mapWithIndex :: MatrixElement f => ((Int, Int) -> e -> f) -> Matrix e -> Matrix f
    allWithIndex :: ((Int, Int) -> e -> Bool) -> Matrix e -> Bool
    anyWithIndex :: ((Int, Int) -> e -> Bool) -> Matrix e -> Bool

    unit n  = fromList [[ if i == j then 1 else 0 | j <- [1..n]] | i <- [1..n] ]
    zero n  = matrix (n,n) (const 0)
    empty   = fromList []
    diag xs = matrix (n,n) (\(i,j) -> if i == j then xs !! (i-1) else 0)
      where n = length xs

    select p m = [ at m (i,j) | i <- [1..numRows m]
                              , j <- [1..numCols m]
                              , p (i,j) ]

    at mat (i, j) = ((!! j) . (!! i) . toList) mat
    
    row i = (!! (i-1)) . toList
    col i = row i . transpose

    numRows = fst . dimensions
    numCols = snd . dimensions
    dimensions m = case toList m of [] -> (0, 0)
                                    (x:xs) -> (length xs + 1, length x)

    adjugate = transpose . cofactors
    transpose mat = matrix (n, m) (\(i,j) -> mat `at` (j,i))
      where (m, n) = dimensions mat
    trace = select (uncurry (==))
    inv _ = Nothing

    minorMatrix (i,j) mat = matrix (numRows mat - 1, numCols mat - 1) $
                \(i',j') -> mat `at` (if i' >= i then i' + 1 else i',
                                      if j' >= j then j' + 1 else j')

    minor ix = det . minorMatrix ix

    cofactors mat = matrix (dimensions mat) $
       \(i,j) -> fromIntegral ((-1 :: Int)^(i+j)) * minor (i,j) mat

    map f = mapWithIndex (const f)
    all f = allWithIndex (const f)
    any f = anyWithIndex (const f)

    mapWithIndex f m = matrix (dimensions m) (\x -> f x (m `at` x))
    allWithIndex f m = P.all id [ f (i, j) (m `at` (i,j))
                                | i <- [1..numRows m], j <- [1..numCols m]]
    anyWithIndex f m = P.any id [ f (i, j) (m `at` (i,j))
                                | i <- [1..numRows m], j <- [1..numCols m]]

    a `plus` b
        | dimensions a /= dimensions b = error "Matrix.plus: dimensions don't match."
        | otherwise = matrix (dimensions a) (\x -> a `at` x + b `at` x)
    a `minus` b
        | dimensions a /= dimensions b = error "Matrix.minus: dimensions don't match."
        | otherwise = matrix (dimensions a) (\x -> a `at` x - b `at` x)
    a `times` b
        | numCols a /= numRows b = error "Matrix.times: `numRows a' and `numCols b' don't match."
        | otherwise = _mult a b


instance MatrixElement Int where
    matrix d g = runST (_matrix IntMatrix arrayST arraySTU d g)
    fromList = _fromList IntMatrix

    at         (IntMatrix _ _ arr) = _at arr
    dimensions (IntMatrix m n _)   = (m, n)
    row i      (IntMatrix _ _ arr) = _row i arr
    col j      (IntMatrix _ _ arr) = _col j arr
    toList     (IntMatrix _ _ arr) = _toList arr
    det        (IntMatrix m n arr) = if m /= n then 0 else runST (_det thawsUnboxed arr)
    rank       (IntMatrix _ _ arr) = runST (_rank thawsBoxed arr)

instance MatrixElement Integer where
    matrix d g = runST (_matrix IntegerMatrix arrayST arrayST d g)
    fromList   = _fromList IntegerMatrix

    at         (IntegerMatrix _ _ arr) = _at arr
    dimensions (IntegerMatrix m n _)   = (m, n)
    row i      (IntegerMatrix _ _ arr) = _row i arr
    col j      (IntegerMatrix _ _ arr) = _col j arr
    toList     (IntegerMatrix _ _ arr) = _toList arr
    det        (IntegerMatrix m n arr) = if m /= n then 0 else runST (_det thawsBoxed arr)
    rank       (IntegerMatrix _ _ arr) = runST (_rank thawsBoxed arr)

instance MatrixElement Float where
    matrix d g = runST (_matrix FloatMatrix arrayST arraySTU d g)
    fromList   = _fromList FloatMatrix

    at         (FloatMatrix _ _ arr) = _at arr
    dimensions (FloatMatrix m n _  ) = (m, n)
    row i      (FloatMatrix _ _ arr) = _row i arr
    col j      (FloatMatrix _ _ arr) = _col j arr
    toList     (FloatMatrix _ _ arr) = _toList arr
    det        (FloatMatrix m n arr) = if m /= n then 0 else runST (_det thawsUnboxed arr)
    rank       (FloatMatrix _ _ arr) = runST (_rank thawsBoxed arr)
    inv        (FloatMatrix m n arr) = if m /= n then Nothing else
                                         let x = runST (_inv unboxedST pivotMax arr)
                                         in maybe Nothing (Just . FloatMatrix m n) x

instance MatrixElement Double where
    matrix d g = runST (_matrix DoubleMatrix arrayST arraySTU d g)
    fromList   = _fromList DoubleMatrix

    at         (DoubleMatrix _ _ arr) = _at arr
    dimensions (DoubleMatrix m n _  ) = (m, n)
    row i      (DoubleMatrix _ _ arr) = _row i arr
    col j      (DoubleMatrix _ _ arr) = _col j arr
    toList     (DoubleMatrix _ _ arr) = _toList arr
    det        (DoubleMatrix m n arr) = if m /= n then 0 else runST (_det thawsUnboxed arr)
    rank       (DoubleMatrix _ _ arr) = runST (_rank thawsBoxed arr)
    inv        (DoubleMatrix m n arr) = if m /= n then Nothing else
                                         let x = runST (_inv unboxedST pivotMax arr)
                                         in maybe Nothing (Just . DoubleMatrix m n) x

instance (Show a, Integral a) => MatrixElement (Ratio a) where
    matrix d g = runST (_matrix RatioMatrix arrayST arrayST d g)
    fromList   = _fromList RatioMatrix

    at         (RatioMatrix _ _ arr) = _at arr
    dimensions (RatioMatrix m n _  ) = (m, n)
    row i      (RatioMatrix _ _ arr) = _row i arr
    col j      (RatioMatrix _ _ arr) = _col j arr
    toList     (RatioMatrix _ _ arr) = _toList arr
    det        (RatioMatrix m n arr) = if m /= n then 0 else  runST (_det thawsBoxed arr)
    rank       (RatioMatrix _ _ arr) = runST (_rank thawsBoxed arr)
    inv        (RatioMatrix m n arr) = if m /= n then Nothing else
                                        let x = runST (_inv boxedST pivotMax arr)
                                        in maybe Nothing (Just . RatioMatrix m n) x

instance (Show a, RealFloat a) => MatrixElement (Complex a) where
    matrix d g = runST (_matrix ComplexMatrix arrayST arrayST d g)
    fromList   = _fromList ComplexMatrix

    at         (ComplexMatrix _ _ arr) = _at arr
    dimensions (ComplexMatrix m n _  ) = (m, n)
    row i      (ComplexMatrix _ _ arr) = _row i arr
    col j      (ComplexMatrix _ _ arr) = _col j arr
    toList     (ComplexMatrix _ _ arr) = _toList arr
    det        (ComplexMatrix m n arr) = if m /= n then 0 else runST (_det thawsBoxed arr)
    rank       (ComplexMatrix _ _ arr) = runST (_rank thawsBoxed arr)
    inv        (ComplexMatrix m n arr) = if m /= n then Nothing else
                                          let x = runST (_inv boxedST pivotNonZero arr)
                                          in maybe Nothing (Just . ComplexMatrix m n) x


_at :: (IArray a (u Int e), IArray u e)
    => a Int (u Int e) -> (Int, Int) -> e
_at arr (i,j) = arr ! i ! j

_row, _col :: (IArray a (u Int e), IArray u e) => Int -> a Int (u Int e) -> [e]
_row i arr = let row = arr ! i in [ row ! j | j <- [1..(snd (bounds row))] ]
_col j arr = [ arr ! i ! j | i <- [1..(snd (bounds arr))] ]

_toList :: (IArray a e) => Array Int (a Int e) -> [[e]]
_toList = P.map elems . elems

_fromList :: (IArray a (u Int e), IArray u e)
          => (Int -> Int -> a Int (u Int e) -> matrix e) -> [[e]] -> matrix e
_fromList c xs =
    let lengths = P.map length xs
        numCols = if null lengths then 0 else foldl1 min lengths
        numRows = length lengths
        
    in  c numRows numCols
          $ array (1, numRows)
          $ zip [1..numRows]
          $ P.map (array (1, numCols) . zip [1..numCols]) xs

thawsBoxed :: (IArray a e, MArray (STArray s) e (ST s))
           => Array Int (a Int e)
           -> ST s [STArray s Int e]
thawsBoxed = mapM thaw . elems

thawsUnboxed :: (IArray a e, MArray (STUArray s) e (ST s))
             => Array Int (a Int e)
             -> ST s [STUArray s Int e]
thawsUnboxed = mapM thaw . elems

arrays :: [(u s) Int e]
       -> ST s ((STArray s) Int ((u s) Int e))
arrays list = newListArray (1, length list) list

augment :: (IArray a e, MArray (u s) e (ST s), Num e)
        => ((Int, Int) -> [e] -> ST s ((u s) Int e))
        -> Array Int (a Int e)
        -> ST s (STArray s Int (u s Int e))
augment _ arr = do
    let (_, n) = bounds arr
        row (a,i) = newListArray (1, 2*n)
                                 [ if j > n then (if j == i + n then 1 else 0)
                                            else a ! j
                                 | j <- [1..2*n] ]
    
    mapM row (zip (elems arr) [1..]) >>= newListArray (1, n)

boxedST :: MArray (STArray s) e (ST s)
        => (Int, Int) -> [e] -> ST s ((STArray s) Int e)
boxedST = newListArray

unboxedST :: MArray (STUArray s) e (ST s)
          => (Int, Int) -> [e] -> ST s ((STUArray s) Int e)
unboxedST = newListArray

arrayST :: MArray (STArray s) e (ST s)
        => (Int, Int) -> e -> ST s ((STArray s) Int e)
arrayST = newArray

arraySTU :: MArray (STUArray s) e (ST s)
         => (Int, Int) -> e -> ST s ((STUArray s) Int e)
arraySTU = newArray


tee :: Monad m => (b -> m a) -> b -> m b
tee f x = f x >> return x

read :: (MArray a1 b m, MArray a (a1 Int b) m) =>
                       a Int (a1 Int b) -> Int -> Int -> m b
read a i j = readArray a i >>= flip readArray j

pivotMax :: Ord v => [(i, v)] -> i
pivotMax = fst . L.maximumBy (compare `on` snd)

pivotNonZero :: (Num v, Eq v) => [(i, v)] -> i
pivotNonZero xs = maybe (fst $ head xs) fst $ L.find ((/= 0) . snd) xs

_inv :: (IArray a e, MArray (u s) e (ST s), Fractional e, Show e, Eq e)
     => ((Int, Int) -> [e] -> ST s ((u s) Int e))
        -- ^ A function for building a new array
     -> ([(Int, e)] -> Int)
        -- ^ A function for selecting pivot elements
     -> Array Int (a Int e)
        -- ^ A matrix as arrays or arrays
     -> ST s (Maybe (Array Int (a Int e)))
_inv mkArrayST selectPivot mat = do
    let m = snd $ bounds mat
        n = 2 * m

        swap a i j = do
            tmp <- readArray a i
            readArray a j >>= writeArray a i
            writeArray a j tmp

    okay <- newSTRef True

    a <- augment mkArrayST mat

    forM_ [1..m] $ \k -> do
        iPivot <- selectPivot <$> zip [k..m]
                              <$> mapM (\i -> abs <$> read a i k) [k..m]

        p <- read a iPivot k
        if p == 0 then writeSTRef okay False else do

            swap a iPivot k

            forM_ [k+1..m] $ \i -> do
                a_i <- readArray a i
                a_k <- readArray a k
                forM_ [k+1..n] $ \j -> do
                    a_ij <- readArray a_i j
                    a_kj <- readArray a_k j
                    a_ik <- readArray a_i k
                    writeArray a_i j (a_ij - a_kj * (a_ik / p))
                writeArray a_i k 0

    invertible <- readSTRef okay

    if invertible then
      do
        forM_ [ m - v | v <- [0..m-1] ] $ \i -> do
            a_i <- readArray a i
            p   <- readArray a_i i
            writeArray a_i i 1
            forM_ [i+1..n] $ \j -> do
                readArray a_i j >>= writeArray a_i j . (/ p)

            unless (i == m) $ do
                forM_ [i+1..m] $ \k -> do
                    a_k <- readArray a k
                    p   <- readArray a_i k

                    forM_ [k..n] $ \j -> do
                        a_ij <- readArray a_i j
                        a_kj <- readArray a_k j
                        writeArray a_i j (a_ij - p * a_kj)

        mapM (\i -> readArray a i >>= getElems
                        >>= return . listArray (1, m) . drop m) [1..m]
            >>= return . Just . listArray (1, m)

      else return Nothing

_rank :: (IArray a e, MArray (u s) e (ST s), Num e, Division e, Eq e)
      => (Array Int (a Int e) -> ST s [(u s) Int e])
      -- ^ A function for thawing a boxed array
      -> Array Int (a Int e)
      -- ^ A matrix given as array of arrays
      -> ST s e
_rank thaws mat = do
    let m = snd $ bounds mat
        n = snd $ bounds (mat ! 1)

        swap a i j = do
            tmp <- readArray a i
            readArray a j >>= writeArray a i
            writeArray a j tmp

    a <- thaws mat >>= arrays

    ixPivot <- newSTRef 1
    prevR   <- newSTRef 1

    forM_ [1..n] $ \k -> do
        pivotRow <- readSTRef ixPivot

        switchRow <- mapM (\i -> read a i k) [pivotRow .. m]
            >>= return . L.findIndex (/= 0)

        when (isJust switchRow) $ do
            let ix = fromJust switchRow + pivotRow
            when (pivotRow /= ix) (swap a pivotRow ix)

            a_p   <- readArray a k
            pivot <- readArray a_p k
            prev  <- readSTRef prevR
            
            forM_ [pivotRow+1..m] $ \i -> do
                a_i <- readArray a i
                forM_ [k+1..n] $ \j -> do
                    a_ij <- readArray a_i j
                    a_ik <- readArray a_i k
                    a_pj <- readArray a_p j
                    writeArray a_i j ((pivot * a_ij - a_ik * a_pj)
                                        `divide` prev)

            writeSTRef ixPivot (pivotRow + 1)
            writeSTRef prevR pivot

    readSTRef ixPivot >>= return . (+ negate 1) . fromIntegral


_det :: (IArray a e, MArray (u s) e (ST s),
         Num e, Eq e, Division e)
     => (Array Int (a Int e) -> ST s [(u s) Int e])
     -> Array Int (a Int e) -> ST s e
_det thaws mat = do

    let size = snd $ bounds mat

    a <- thaws mat >>= arrays

    signR  <- newSTRef 1
    pivotR <- newSTRef 1

    forM_ [1..size] $ \k -> do
        sign <- readSTRef signR
        unless (sign == 0) $ do

            prev  <- readSTRef pivotR
            pivot <- read a k k >>= tee (writeSTRef pivotR)

            when (pivot == 0) $ do
                s <- forM [(k+1)..size] $ \r -> do
                    a_rk <- read a r k
                    if a_rk == 0 then return 0 else return r
                let sf = filter (>0) s

                when (not $ null sf) $ do
                    let sw = head sf

                    row <- readArray a sw
                    readArray a k >>= writeArray a sw
                    writeArray a k row

                    read a k k >>= writeSTRef pivotR
                    readSTRef signR >>= writeSTRef signR . negate

                when (null sf) (writeSTRef signR 0)

            sign' <- readSTRef signR
            unless (sign' == 0) $ do
                pivot' <- readSTRef pivotR
                forM_ [(k+1)..size] $ \i -> do
                    a_i <- readArray a i
                    forM [(k+1)..size] $ \j -> do
                        a_ij <- readArray a_i j
                        a_ik <- readArray a_i k
                        a_kj <- read a k j
                        writeArray a_i j ((pivot' * a_ij - a_ik * a_kj) `divide` prev)

    liftM2 (*) (readSTRef pivotR) (readSTRef signR)


-- TODO: More efficient implementation (decrease the constant factors
-- a little bit by working in the ST monad)
-- [ remark: not sure if this will be faster than lists -> benchmark! ]
_mult :: MatrixElement e => Matrix e -> Matrix e -> Matrix e
_mult a b = let rowsA = numRows a
                rowsB = numRows b
                colsB = numCols b
            in  matrix (rowsA, colsB) (\(i,j) -> L.foldl' (+) 0 [a `at` (i, k) * b `at` (k, j) | k <- [1..rowsB]])


_matrix :: (IArray a1 (u Int e), IArray u e,
            MArray a2 (u Int e) (ST s), MArray a3 e (ST s),
            Num e)
        => (Int -> Int -> a1 Int (u Int e) -> matrix)
        -> ((Int, Int) -> a -> ST s (a2 Int (u Int e)))
        -> ((Int, Int) -> e -> ST s (a3 Int e))
        -> (Int, Int)
        -> ((Int, Int) -> e)
        -> ST s matrix
_matrix c newArray newArrayU (m, n) g = do
    rows <- newArray (1, m) undefined
    forM_ [1..m] $ \i -> do
        cols <- newArrayU (1, n) 0
        forM_ [1..n] $ \j -> do
            writeArray cols j (g (i,j))
        U.unsafeFreeze cols >>= writeArray rows i
    U.unsafeFreeze rows >>= return . c m n


{-# RULES

"det/pow"
    forall a k. det (a ^ k) = (det a) ^ k

 #-}
