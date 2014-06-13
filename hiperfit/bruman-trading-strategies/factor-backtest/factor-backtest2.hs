{- 
DSH implementation of trading strategy 'factor backtest' from Filip Bruman's thesis.
This variant does not rely on list semantics to store the matrix, but keeps the
logical order in form of the cell row/column numbers. This way, it avoids quite a
lot of sorting.

However, this forces us to employ proper joins instead of zips to align lists. On the
X100 implementation, this might be more costly.
-}        

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL
import Database.X100Client

data Cell = Cell { row :: Integer, col :: Integer, content :: Double }

deriveDSH ''Cell
deriveTA ''Cell
generateTableSelectors ''Cell

type Row = [(Integer, Double)]

storage :: String -> Q [Cell]
storage tab = table tab defaultHints { keysHint = [Key ["r", "c"]]
                                     , nonEmptyHint = NonEmpty
                                     }


-- Try grouping instead of a nestjoin.
mkMatrix :: Q [Cell] -> Q [(Integer, Row)]
mkMatrix raw = 
    [ tuple2 r ([ tuple2 (colQ c) (contentQ c) | c <- raw, rowQ c == r ])
    | r <- nub $ map rowQ raw
    ]

spr :: Q Row -> Q Row -> Q Row 
spr as bs = [ tuple2 i1 ((b - a) / a) | (view -> (i1, a)) <- as
                                      , (view -> (i2, b)) <- bs
                                      , i1 == i2
                                      ]

buckets :: Integer -> Integer -> Q [(Integer, Double)] -> Q ([Integer], [Integer])
buckets k1 k2 r = tuple2 topBucket bottomBucket
  where 
    topBucket    = drop ((length rs) - (toQ k1)) rs
    bottomBucket = take (toQ k2) rs
    rs           = map fst $ sortWith snd r
        
sumIndex :: Q Row -> Q [Integer] -> Q Double
sumIndex r b = sum [ v | (view -> (i, v)) <- r, i `elem` b ]

price :: Q Double -> Q Row -> Q [Integer] -> Q [Integer] -> Q Double
price w r b1 b2 = sumIndex r b1 + w * sumIndex r b2
      
align :: Integer -> Q [(Integer, Row)] -> Q [(Row, Row, Row)]
align s m = [ tuple3 r1 r2 r3 | (view -> (i1, r1)) <- m
                              , (view -> (i2, r2)) <- m
                              , (view -> (i3, r3)) <- m
                              , i2 == i1 + (toQ s)
                              , i3 == i1 + 2 * (toQ s)
                              , (i1 `mod` (toQ s)) == 0
                              ]

backtest :: Integer -> Q [(Integer, Row)] -> Integer -> Integer -> Integer -> Q [Double]
backtest m matrix k1 k2 s = map go (align s matrix')
  where 
    matrix' = drop (toQ $ m `P.mod` s) matrix
        
    go :: Q (Row, Row, Row) -> Q Double
    go (view -> (ri, ris, ri2s)) =
      let r = spr ri ris
          (view -> (b1, b2)) = buckets k1 k2 r
          w = sumIndex ris b1 / sumIndex ris b2
      in (price w ri2s b1 b2) - (price w ris b1 b2)

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' dbname = 'au'"

getConnX100 :: X100Info
getConnX100 = x100Info "localhost" "48130" Nothing

main :: IO ()
main = debugQX100 "factor-backtest_sparse" getConnX100 $ backtest 2 (mkMatrix $ storage "foo") 2 2 5
