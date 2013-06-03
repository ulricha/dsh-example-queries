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
import Database.DSH.Flattening

import Database.X100Client

type Row = [(Integer, Double)]

storage :: Q [(Integer, Integer, Double)]
storage = toQ [ (1,1,10)
              , (1,2,20)
              , (1,3,30)
              , (2,1,40)
              , (2,2,50)
              , (2,3,60)
              , (3,1,70)
              , (3,2,80)
              , (3,3,90)
              ]

matrix :: Q [(Integer, Row)]
matrix = let rows = nub $ map (\(view -> (r, _, _)) -> r) storage
         in [ tuple2 r ([ tuple2 c v | (view -> (r', c, v)) <- storage, r == r' ])
            | r <- rows
            ]

spr :: Q Row -> Q Row -> Q Row 
spr as bs = [ tuple2 i1 ((b - a) / a) | (view -> (i1, a)) <- as
                                      , (view -> (i2, b)) <- bs
                                      , i1 == i2
                                      ]

buckets :: Q Integer -> Q Integer -> Q [(Integer, Double)] -> Q ([Integer], [Integer])
buckets k1 k2 r = tuple2 topBucket bottomBucket
  where topBucket    = drop ((length rs) - k1) rs
        bottomBucket = take k2 rs
        rs           = map fst $ sortWith snd r
        
sumIndex :: Q Row -> Q [Integer] -> Q Double
sumIndex row b = sum [ v | (view -> (i, v)) <- row, i `elem` b ]

price :: Q Double -> Q Row -> Q [Integer] -> Q [Integer] -> Q Double
price w row b1 b2 = sumIndex row b1 + w * sumIndex row b2
      
align :: Q Integer -> Q [(Integer, Row)] -> Q [(Row, Row, Row)]
align s m = [ tuple3 r1 r2 r3 | (view -> (i1, r1)) <- m
                              , (view -> (i2, r2)) <- m
                              , (view -> (i3, r3)) <- m
                              , i2 == i1 + s
                              , i3 == i1 + 2 * s
                              , (i1 `mod` s) == 0
                              ]

backtest :: Q [(Integer, Row)] -> Q Integer -> Q Integer -> Q Integer -> Q [Double]
backtest m k1 k2 s = map go (align s m')
  where m' = drop (length m `mod` s) m
        
        go :: Q (Row, Row, Row) -> Q Double
        go (view -> (ri, ris, ri2s)) =
          let r = spr ri ris
              (view -> (b1, b2)) = buckets k1 k2 r
              w = sumIndex ris b1 / sumIndex ris b2
          in (price w ri2s b1 b2) - (price w ris b1 b2)

-- getConn :: IO Connection
-- getConn = connectPostgreSQL "user = 'giorgidz' password = '' host = 'localhost' dbname = 'giorgidz'"

getConn :: IO X100Info
getConn = P.return $ x100Info "localhost" "48130" Nothing

runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> fromQX100 conn q P.>>= P.print

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugX100VL "factor-backtest2" conn q

main :: IO ()
main = debugQ $ backtest matrix (toQ 2) (toQ 2) (toQ 5)
