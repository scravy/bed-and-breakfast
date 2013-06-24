{-# LANGUAGE Haskell2010, TemplateHaskell #-}

module Numeric.Matrix.Sugar (
        iMatrix, dMatrix
    ) where

import Numeric.Matrix
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Data.Data
import Data.Complex
import Data.Ratio

type ReadM a = String -> Matrix a

iMatrix = QuasiQuoter
            (quoter (read :: ReadM Integer))
            undefined undefined undefined

dMatrix = QuasiQuoter
            (quoter (read :: ReadM Double))
            undefined undefined undefined

quoter :: (Data e, MatrixElement e)
       => (String -> Matrix e) -> String -> Q Exp

quoter read str = do
    let qExp = dataToExpQ (const Nothing) (toList (read str))
    [e| fromList $qExp |]

