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

module Queries.TPCH.Q2
    ( q2
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Queries.TPCH.Records

minSupplyCost :: Q Integer -> Q Double
minSupplyCost partkey = 
  minimum $ 
  [ ps_supplycostQ ps
  | ps <- partsupps
  , s  <- suppliers
  , n  <- nations
  , r  <- regions
  , partkey == ps_partkeyQ ps
  , s_suppkeyQ s == ps_suppkeyQ ps
  , s_nationkeyQ s == n_nationkeyQ n
  , n_regionkeyQ n == r_regionkeyQ r
  , r_nameQ r == (toQ "EUROPE")
  ]

sortingCriteria
  :: Q (Double, Text, Text, Integer, Text, Text, Text, Text)
  -> Q (Double, Text, Text, Integer)
sortingCriteria (view -> (b, sn, nn, pk, _, _, _, _)) =
  tuple4 (b * (toQ $ -1.0)) nn sn pk

q2 :: Q [(Double, Text, Text, Integer, Text, Text, Text, Text)]
q2 = 
  sortWith sortingCriteria $
  [ tuple8 (s_acctbalQ s)
           (s_nameQ s)
	   (n_nameQ n)
	   (p_partkeyQ p)
	   (p_mfgrQ p)
	   (s_addressQ s)
	   (s_phoneQ s)
	   (s_commentQ s)
  | p  <- parts
  , ps <- partsupps
  , s  <- suppliers
  , n  <- nations
  , r  <- regions
  , p_partkeyQ p == ps_partkeyQ ps
  , s_suppkeyQ s == ps_suppkeyQ ps
  , p_sizeQ p == (toQ 15)
  , p_typeQ p `like` (toQ "%BRASS")
  , s_nationkeyQ s == n_nationkeyQ n
  , n_regionkeyQ n == r_regionkeyQ r
  , r_nameQ r == (toQ "EUROPE")
  , ps_supplycostQ ps == minSupplyCost (p_partkeyQ p)
  ]
