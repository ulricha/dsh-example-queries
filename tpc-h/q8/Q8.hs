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
    
-- TPC-H Q8

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

between :: Q Integer -> Q Integer -> Q Integer -> Q Bool
between x l r = x >= l && x <= r

allNations :: Q [(Integer, Double, Text)]
allNations =
  [ tuple3 (o_orderdateQ o) -- should extract the year
  	   (l_extendedpriceQ l * (1 - l_discountQ l))
	   (n_nameQ n2)
  | p <- parts
  , s <- suppliers
  , l <- lineitems
  , o <- orders
  , c <- customers
  , n1 <- nations
  , n2 <- nations
  , r <- regions
  , p_partkeyQ p == l_partkeyQ l
  , s_suppkeyQ s == l_suppkeyQ l
  , l_orderkeyQ l == o_orderkeyQ o
  , o_custkeyQ o == c_custkeyQ c
  , c_nationkeyQ c == n_nationkeyQ n1
  , n_regionkeyQ n1 == r_regionkeyQ r
  , r_nameQ r == "AMERICA"
  , s_nationkeyQ s == n_nationkeyQ n2
  , between (o_orderdateQ o) 23 42
  , p_typeQ p == "ECONOMY ANODIZED STEEL"
  ] 

q8 = 
  sortWith fst
  [ pair k
         ((sum [ if n == "BRAZIL"
                 then v
		 else 0
	       | (view -> (_, v, n)) <- g
	       ])
	  / sum [ v | (view -> (_, v, _)) <- g ])
  | (view -> (k, g)) <- groupWithKey (\(view -> (n, _, _)) -> n) allNations
  ]
  
       

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugTAOpt "q8" conn q

main :: IO ()
main = debugQ q8
