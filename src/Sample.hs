{-# LANGUAGE Haskell2010, TemplateHaskell, QuasiQuotes #-}
--{-# OPTIONS -Wall #-}


import Data.Maybe
import System.Random
import Numeric.Matrix hiding (map)
import qualified Numeric.Matrix as M
import Data.Ratio

import Numeric.Matrix.Sugar

some = [iMatrix| 23 4 5
                 17 3 4
                 90 0 9 |] :: Matrix Rational

m :: Matrix Rational

m = fromList
        [[2,-2,1,1,0,0]
        ,[0,0,2,1,9,2]
        ,[1,1,0,-2,4,3]
        ,[-2,2,-2,2,3,2]
        ,[-2,2,-1,1,78,2]
        ,[-8,12,0,9,-5,2]]

m' = (m <|> m) <-> (m <|> m)

f = det (m ^ 30000)
g = m * M.map (+2) m + m * M.map (+3) m

main = print f >> print g
