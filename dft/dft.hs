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
ρ :: QA a => Integer -> Integer -> Q [a] -> Q [[a]]
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
                       | (view -> (z, a)) <- number $ φ $ ρ m n v
                       ]
         | d <- toQ [ 0 .. m - 1 ]
         ] 

dftFastRec :: Integer -> Integer -> Q [Double] -> Q [Double]
dftFastRec m n v =
  concat [ map sum $ φ [ [ ω (m * n) (a * (toQ n * d + c)) * t
                         | (view -> (t, c)) <- number $ dft n z
                         ]
                       | (view -> (z, a)) <- number $ φ $ ρ m n v
                       ]
         | d <- toQ [ 0 .. m - 1 ]
         ] 


---------------------------------------------------------------------
-- Sparse vector implementation

-- FIXME assume here that groupWith keeps order in partitions stable,
-- i.e. sorts by pos. That might currently not be the case.
reshape n :: QA a => Q [(Int, a)] -> Q [(Int, [(Int, a)])]
reshape n vec = -- Generate new indices for the outer vector
                number 
		-- Generate new indices for the inner vectors
		$ map number 
		-- Throw away old vector indices
		$ map (map snd) 
		$ groupWith byPos $ number vec

  where
    byPos :: QA a => Q (Integer, a) -> Q Integer
    byPos v = ((fst v) - 1) / n

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: QA a => String -> Q a -> IO ()
debugQ prefix q = getConn P.>>= \conn -> debugVL prefix conn q

main :: IO ()
main =
    debugQ "dft" (dft 5 (toQ [1,2,3,4,5]))
    P.>>
    debugQ "dftFast" (dftFast 2 4 (toQ [1,2,3,4,5,6,7,8]))
