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

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

-- | Matrix transposition
φ :: QA a => Q [[a]] -> Q [[a]]
φ = transpose

-- | Array reshaping
ρ :: QA a => Integer -> Q [a] -> Q [[a]]
ρ = reshape

iArray :: QA a => Q [a] -> Q [Integer]
iArray v = map snd $ number v

principalRoot :: Integer -> Integer
principalRoot _ = undefined

power :: Q Integer -> Q Integer -> Q Integer
power = undefined

ω :: Integer -> Q Integer -> Q Double
-- ω n i = power (toQ (principalRoot n)) i
ω _ i = toQ 42 + (integerToDouble i)


dft :: Integer -> Q [Double] -> Q [Double]
dft dim v = 
  [ sum [ ω dim (i * j) * x | (view -> (x, j)) <- number v ]
  | i <- iArray v
  ]

dftFast :: Integer -> Integer -> Q [Double] -> Q [Double]
dftFast m n v =
  concat [ map sum $ φ [ [ ω (m * n) (a * (toQ n * d + c)) * t
                         | (view -> (t, c)) <- number $ dft n z
                         ]
                       | (view -> (z, a)) <- number $ φ $ ρ n v
                       ]
         | d <- toQ [ 0 .. m - 1 ]
         ] 

dftFastRec :: Integer -> Integer -> Q [Double] -> Q [Double]
dftFastRec m n v =
  concat [ map sum $ φ [ [ ω (m * n) (a * (toQ n * d + c)) * t
                         | (view -> (t, c)) <- number $ dft n z
                         ]
                       | (view -> (z, a)) <- number $ φ $ ρ n v
                       ]
         | d <- toQ [ 0 .. m - 1 ]
         ] 


---------------------------------------------------------------------
-- Sparse vector implementation

-- FIXME assume here that groupWith keeps order in partitions stable,
-- i.e. sorts by pos. That might currently not be the case.
-- reshapeSparse :: QA a => Integer -> Q [(Int, a)] -> Q [(Int, [(Int, a)])]
-- reshapeSparse n vec = 
--     -- Generate new indices for the outer vector
--     number 
--     -- Generate new indices for the inner vectors
--     $ map number 
--     -- Throw away old vector indices
--     $ map (map snd) 
--     $ groupWith byPos $ number vec
-- 
--   where
--     byPos :: QA a => Q (Integer, a) -> Q Integer
--     byPos v = ((fst v) - 1) / n

type SparseVector a = [(Integer, a)]

type Row a = (Integer, Integer, a)
 
type SparseMatrix a = [Row a]

row :: QA a => Q (Row a) -> Q Integer
row (view -> (r, _, _)) = r

rowVec :: QA a => Q Integer -> Q (SparseMatrix a) -> Q (SparseVector a)
rowVec r m = [ pair c x | (view -> (r', c, x)) <- m, r == r' ]

reshape2 :: QA a => Integer -> Q (SparseVector a) -> Q (SparseMatrix a)
reshape2 n v = 
  [ tuple3 (p `div` (toQ n)) (p `mod` (toQ n)) x
  | (view -> (p, x)) <- v
  ]

transpose2 :: QA a => Q (SparseMatrix a) -> Q (SparseMatrix a)
transpose2 m =
  [ tuple3 c r x
  | (view -> (r, c, x)) <- m
  ]

dftSparse :: Integer -> Q (SparseVector Double) -> Q (SparseVector Double)
dftSparse dim v = 
  [ pair i (sum $ map snd xs)
  | (view -> (i, xs)) <- groupWithKey fst 
                                   [ pair i (ω dim (i * j) * x)
                                   | i <- (map fst v)
                                   , (view -> (j, x)) <- v
                                   ]
  ]

-- dftFastSparse :: Integer -> Integer -> Q (SparseVector Double) -> Q (SparseVector Double)
-- dftFastSparse m n v
--   [ [ 
--     | (view -> (t, c)) <- dftSparse $ dftrowVec a w
--     ]
--   | d <- toQ [ 0 .. m - 1 ]
--   , let w = transpose2 $ reshape2 n v
--   , a <- nub $ map row w
--   ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: QA a => String -> Q a -> IO ()
debugQ prefix q = getConn P.>>= \conn -> debugVL prefix conn q

main :: IO ()
main =
    debugQ "dft" (dft 5 (toQ [1,2,3,4,5]))
    P.>>
    debugQ "dftFast" (dftFast 2 4 (toQ [1,2,3,4,5,6,7,8]))
