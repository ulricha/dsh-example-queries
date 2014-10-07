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

module Queries.TPCH.Q8
    ( q8
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

data Interval = Interval { iv_start :: Integer, iv_end :: Integer }

inInterval :: Q Integer -> Interval -> Q Bool
inInterval d interval = d >= toQ (iv_start interval) && d < toQ (iv_end interval)

between :: Q Integer -> Q Integer -> Q Integer -> Q Bool
between x l r = x >= l && x <= r

revenue :: Q LineItem -> Q Double
revenue l = l_extendedpriceQ l * (1 - l_discountQ l)

-- FIXME should extract the year
allNations :: Text -> Text -> Interval -> Q [(Integer, Double, Text)]
allNations region typ interval =
  [ tup3 (o_orderdateQ o) (revenue l) (n_nameQ n2)
  | p  <- parts
  , s  <- suppliers
  , l  <- lineitems
  , o  <- orders
  , c  <- customers
  , n1 <- nations
  , n2 <- nations
  , r  <- regions
  , p_partkeyQ p == l_partkeyQ l
  , s_suppkeyQ s == l_suppkeyQ l
  , l_orderkeyQ l == o_orderkeyQ o
  , o_custkeyQ o == c_custkeyQ c
  , c_nationkeyQ c == n_nationkeyQ n1
  , n_regionkeyQ n1 == r_regionkeyQ r
  , r_nameQ r == toQ region
  , s_nationkeyQ s == n_nationkeyQ n2
  , o_orderdateQ o `inInterval` interval
  , p_typeQ p == toQ typ
  ] 

nationVolumne :: Text -> Q [(Integer, Double, Text)] -> Q Double
nationVolumne nation salesInYear =
    sum [ if n == "BRAZIL" then v else 0
	| (view -> (_, v, n)) <- salesInYear
	]

completeVolume :: Q [(Integer, Double, Text)] -> Q Double
completeVolume salesInYear =
    sum [ v | (view -> (_, v, n)) <- salesInYear ]

q8 nation = 
  sortWith fst
  [ tup2 k (nationVolumne nation g / completeVolume g)
  | (view -> (k, g)) <- groupWithKey (\(view -> (n, _, _)) -> n) allNations
  ]
