{- 
DSH implementation of trading strategy 'factor backtest' from Filip Bruman's thesis.
This variant relies on a list semantics, i.e. the matrix elements are kept in proper 
order.
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

storage :: Q [Cell]
storage = undefined

data Cell = Cell
  { row :: Integer
  , col :: Integer
  , val :: Double
  }
  
deriveDSH ''Cell
generateTableSelectors ''Cell

matrix :: Q [[Double]]
matrix = map (sortRow . snd) $ sortWith fst $ groupWithKey rowQ storage
  where sortRow :: Q [Cell] -> Q [Double]
        sortRow cs = map valQ $ sortWith colQ cs

zipIndex :: QA a => Q [a] -> Q [(Integer, a)]
zipIndex xs = zip (undefined xs) xs

spr :: Q [Double] -> Q [Double] -> Q [(Integer, Double)]
spr as bs = map (\(view -> (i, p)) -> tuple2 i (ret p)) $ zipIndex $ zip as bs
  where ret :: Q (Double, Double) -> Q Double
        ret (view -> (a, b)) = (b - a) / a

buckets :: Q Integer -> Q Integer -> Q [(Integer, Double)] -> Q ([Integer], [Integer])
buckets k1 k2 r = tuple2 topBucket bottomBucket
  where topBucket    = map fst $ take k1 $ reverse rs
        bottomBucket = map fst $ take k2 rs
        rs           = sortWith snd r
        
sumIndex :: Q [Double] -> Q [Integer] -> Q Double
sumIndex row b = sum [ v | (view -> (i, v)) <- zipIndex row, i `elem` b ]

price :: Q Double -> Q [Double] -> Q [Integer] -> Q [Integer] -> Q Double
price w row b1 b2 = sumIndex row b1 + w * sumIndex row b2
      
align :: Q Integer -> Q [[Double]] -> Q [(([Double], [Double]), [Double])]
align s m = map snd $ filter multiples $ zipIndex $ zip (zip m (drop s m)) (drop (2*s) m)

  where multiples :: QA r => Q (Integer, r) -> Q Bool
        multiples p = ((fst p) `mod` s) == 0

backtest :: Q [[Double]] -> Q Integer -> Q Integer -> Q Integer -> Q [Double]
backtest m k1 k2 s = map go (align s m')
  where m' = drop (length m `mod` s) m
        
        go :: Q (([Double], [Double]), [Double]) -> Q Double
        go (view -> ((view -> (ri, ris)), ri2s)) =
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
debugQ q = getConn P.>>= \conn -> debugX100VL "q1" conn q

main :: IO ()
main = debugQ $ backtest matrix (toQ 2) (toQ 2) (toQ 5)
