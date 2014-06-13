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
import Database.DSH.Compiler

import Database.X100Client

data Cell = Cell { row :: Integer, col :: Integer, content :: Double }

deriveDSH ''Cell
deriveTA ''Cell
generateTableSelectors ''Cell

rawData :: String -> Q [Double]
rawData tab = map contentQ
              $ sortWith (\c -> pair (rowQ c) (colQ c))
              $ table tab defaultHints { keysHint = [Key ["r", "c"]]
                                       , nonEmptyHint = NonEmpty
                                       }

mkMatrix :: Integer -> Q [Double] -> Q [[Double]]
mkMatrix = reshape

spr :: Q [Double] -> Q [Double] -> Q [(Integer, Double)]
spr as bs = map (\(view -> (p, i)) -> tuple2 i (ret p)) $ number $ zip as bs
  where 
    ret :: Q (Double, Double) -> Q Double
    ret (view -> (a, b)) = (b - a) / a

-- FIXME could be cheaper to drop instead of reverse + take
buckets :: Integer -> Integer -> Q [(Integer, Double)] -> Q ([Integer], [Integer])
buckets k1 k2 r = tuple2 topBucket bottomBucket
  where 
    topBucket    = map fst $ take (toQ k1) $ reverse rs
    bottomBucket = map fst $ take (toQ k2) rs
    rs           = sortWith snd r
        
sumIndex :: Q [Double] -> Q [Integer] -> Q Double
sumIndex r b = sum [ v | (view -> (v, i)) <- number r, i `elem` b ]

price :: Q Double -> Q [Double] -> Q [Integer] -> Q [Integer] -> Q Double
price w r b1 b2 = sumIndex r b1 + w * sumIndex r b2
      
align :: Integer -> Q [[Double]] -> Q [(([Double], [Double]), [Double])]
align s matrix = 
    map fst 
    $ filter multiples 
    $ number 
    $ zip (zip matrix (drop (toQ s) matrix)) (drop (toQ $ 2 P.* s) matrix)

  where 
    multiples :: QA r => Q (r, Integer) -> Q Bool
    multiples p = ((snd p) `mod` (toQ s)) == 0

backtest :: Integer -> Q [[Double]] -> Integer -> Integer -> Integer -> Q [Double]
backtest m matrix k1 k2 s = map go (align s matrix')
  where 
    matrix' = drop (toQ $ m `P.mod` s) matrix
        
    go :: Q (([Double], [Double]), [Double]) -> Q Double
    go (view -> ((view -> (ri, ris)), ri2s)) =
        let r = spr ri ris
            (view -> (b1, b2)) = buckets k1 k2 r
            w = sumIndex ris b1 / sumIndex ris b2
        in (price w ri2s b1 b2) - (price w ris b1 b2)

getConn :: X100Info
getConn = x100Info "localhost" "48130" Nothing

main :: IO ()
main = debugQX100 "factor-backtest_rs" getConn $ backtest 4 matrix 2 2 5
  where
    matrix = mkMatrix 500 $ rawData "10p6_500"
