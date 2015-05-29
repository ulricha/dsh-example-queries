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
    , q2a
    , q2Default
    , q2aDefault
    ) where

import qualified Data.Text as T

import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

-- | TPC-H Query Q2 with standard validation parameters
q2Default :: Q [(Decimal, Text, Text, Integer, Text, Text, Text, Text)]
q2Default = q2 15 "BRASS" "EUROPE"

-- | TPC-H Query Q2 with standard validation parameters (alternative
-- formulation)
q2aDefault :: Q [(Decimal, Text, Text, Integer, Text, Text, Text, Text)]
q2aDefault = q2a 15 "BRASS" "EUROPE"

-- | FIXME implement proper descending sorting
sortingCriteria :: Q (Decimal, Text, Text, Integer, Text, Text, Text, Text)
                -> Q (Decimal, Text, Text, Integer)
sortingCriteria (view -> (b, sn, nn, pk, _, _, _, _)) =
  tup4 (b * (toQ $ -1.0)) nn sn pk

--------------------------------------------------------------------------------

partSuppliersFrom :: Q Part -> Text -> Q [(PartSupp, Supplier, Nation)]
partSuppliersFrom p regionName =
    [ tup3 ps s n
    | (view -> (s, ps)) <- partSuppliers p
    , r <- regions
    , r_nameQ r == toQ regionName
    , n <- regionNations r
    , s_nationkeyQ s == n_nationkeyQ n
    ]

minSupplyCost' :: Text -> Q Part -> Q Decimal
minSupplyCost' regionName p =
    minimum [ ps_supplycostQ ps
            | (view -> (ps, _, _)) <- partSuppliersFrom p regionName
            ]

-- | A less literal implementation of TPC-H Query Q2 with more abstraction.
-- Validation parameters: 15, "BRASS", "EUROPE"
q2 :: Integer
   -> Text
   -> Text
   -> Q [(Decimal, Text, Text, Integer, Text, Text, Text, Text)]
q2 size typ regionName =
  take 100 $
  sortWith sortingCriteria $
  [ tup8 (s_acctbalQ s)
         (s_nameQ s)
         (n_nameQ n)
         (p_partkeyQ p)
         (p_mfgrQ p)
         (s_addressQ s)
         (s_phoneQ s)
         (s_commentQ s)
  | p <- parts
  , (view -> (ps, s, n)) <- partSuppliersFrom p regionName
  , p_typeQ p `like` (toQ $ T.cons '%' typ)
  , p_sizeQ p == (toQ size)
  , ps_supplycostQ ps == minSupplyCost' regionName p
  ]

--------------------------------------------------------------------------------

minSupplyCost :: Text -> Q Integer -> Q Decimal
minSupplyCost regionName partkey =
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
  , r_nameQ r == (toQ regionName)
  ]

-- | A rather literal implementation of TPC-H Query Q2.
-- Validation parameters: 15, "BRASS", "EUROPE"
q2a :: Integer
    -> Text
    -> Text
    -> Q [(Decimal, Text, Text, Integer, Text, Text, Text, Text)]
q2a size typ regionName =
  take 100 $
  sortWith sortingCriteria $
  [ tup8 (s_acctbalQ s)
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
  , p_sizeQ p == (toQ size)
  , p_typeQ p `like` (toQ $ T.cons '%' typ)
  , s_nationkeyQ s == n_nationkeyQ n
  , n_regionkeyQ n == r_regionkeyQ r
  , r_nameQ r == (toQ regionName)
  , ps_supplycostQ ps == minSupplyCost regionName (p_partkeyQ p)
  ]
