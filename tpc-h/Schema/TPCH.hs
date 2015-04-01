{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ViewPatterns          #-}

module Schema.TPCH where

import qualified Prelude as P
import Prelude(Show, Integer)
import Database.DSH
import Data.List.NonEmpty

sng :: a -> NonEmpty a
sng = P.return

-- primary key: l_orderkey, l_linenumber
data LineItem = LineItem
    { l_comment       :: Text
    , l_commitdate    :: Day
    , l_discount      :: Decimal
    , l_extendedprice :: Decimal
    , l_linenumber    :: Integer
    , l_linestatus    :: Text
    , l_orderkey      :: Integer
    , l_partkey       :: Integer
    , l_quantity      :: Decimal
    , l_receiptdate   :: Day
    , l_returnflag    :: Text
    , l_shipdate      :: Day
    , l_shipinstruct  :: Text
    , l_shipmode      :: Text
    , l_suppkey       :: Integer
    , l_tax           :: Decimal
    }
    deriving (Show)

deriveDSH ''LineItem
deriveTA ''LineItem
generateTableSelectors ''LineItem

data Supplier = Supplier
    { s_acctbal   :: Decimal
    , s_address   :: Text
    , s_comment   :: Text
    , s_name      :: Text
    , s_nationkey :: Integer
    , s_phone     :: Text
    , s_suppkey   :: Integer
    }
    deriving (Show)

deriveDSH ''Supplier
deriveTA ''Supplier
generateTableSelectors ''Supplier

data Part = Part
    { p_brand       :: Text
    , p_comment     :: Text
    , p_container   :: Text
    , p_mfgr        :: Text
    , p_name        :: Text
    , p_partkey     :: Integer
    , p_retailprice :: Decimal
    , p_size        :: Integer
    , p_type        :: Text
    }
    deriving (Show)

deriveDSH ''Part
deriveTA ''Part
generateTableSelectors ''Part

data PartSupp = PartSupp
    { ps_availqty   :: Integer
    , ps_comment    :: Text
    , ps_partkey    :: Integer
    , ps_suppkey    :: Integer
    , ps_supplycost :: Decimal
    }
    deriving (Show)

deriveDSH ''PartSupp
deriveTA ''PartSupp
generateTableSelectors ''PartSupp

data Nation = Nation
    { n_comment   :: Text
    , n_name      :: Text
    , n_nationkey :: Integer
    , n_regionkey :: Integer
    }
    deriving (Show)

deriveDSH ''Nation
deriveTA ''Nation
generateTableSelectors ''Nation

data Region = Region
    { r_comment   :: Text
    , r_name      :: Text
    , r_regionkey :: Integer
    }
    deriving (Show)

deriveDSH ''Region
deriveTA ''Region
generateTableSelectors ''Region

data Order = Order
    { o_clerk         :: Text
    , o_comment       :: Text
    , o_custkey       :: Integer
    , o_orderdate     :: Day
    , o_orderkey      :: Integer
    , o_orderpriority :: Text
    , o_orderstatus   :: Text
    , o_shippriority  :: Integer
    , o_totalprice    :: Decimal
    }
    deriving (Show)

deriveDSH ''Order
deriveTA ''Order
generateTableSelectors ''Order

data Customer = Customer
    { c_acctbal    :: Decimal
    , c_address    :: Text
    , c_comment    :: Text
    , c_custkey    :: Integer
    , c_mktsegment :: Text
    , c_name       :: Text
    , c_nationkey  :: Integer
    , c_phone      :: Text
    }
    deriving (Show)

deriveDSH ''Customer
deriveTA ''Customer
generateTableSelectors ''Customer

parts :: Q [Part]
parts = table "part" ( "p_brand" :|
                     [ "p_comment"
                     , "p_container"
                     , "p_mfgr"
                     , "p_name"
                     , "p_partkey"
                     , "p_retailprice"
                     , "p_size"
                     , "p_type"
                     ])
                     (TableHints (sng $ Key (sng "p_partkey") ) NonEmpty)

suppliers :: Q [Supplier]
suppliers = table "supplier"
                  ( "s_acctbal" :|
                  [ "s_address"
                  , "s_comment"
                  , "s_name"
                  , "s_nationkey"
                  , "s_phone"
                  , "s_suppkey"
                  ])
                  (TableHints ( sng $  Key (sng "s_suppkey") ) NonEmpty)

partsupps :: Q [PartSupp]
partsupps = table "partsupp"
                  ( "ps_availqty" :|
                  [ "ps_comment"
                  , "ps_partkey"
                  , "ps_suppkey"
                  , "ps_supplycost"
                  ])
                  (TableHints (sng $ Key ("ps_partkey" :| ["ps_suppkey"])) NonEmpty)

nations :: Q [Nation]
nations = table "nation"
                ( "n_comment" :|
                [ "n_name"
                , "n_nationkey"
                , "n_regionkey"
                ])
                (TableHints (sng $ Key (sng "n_nationkey")) NonEmpty)

regions :: Q [Region]
regions = table "region"
                ( "r_comment" :|
                [ "r_name"
                , "r_regionkey"
                ])
                (TableHints (sng $ Key (sng "r_regionkey")) NonEmpty)

orders :: Q [Order]
orders = table "orders"
               ( "o_clerk" :|
               [ "o_comment"
               , "o_custkey"
               , "o_orderdate"
               , "o_orderkey"
               , "o_orderpriority"
               , "o_orderstatus"
               , "o_shippriority"
               , "o_totalprice"
               ])
               (TableHints (sng $ Key (sng "o_orderkey")) NonEmpty)

lineitems :: Q [LineItem]
lineitems = table "lineitem"
                  ( "l_comment" :|
                  [ "l_commitdate"
                  , "l_discount"
                  , "l_extendedprice"
                  , "l_linenumber"
                  , "l_linestatus"
                  , "l_orderkey"
                  , "l_partkey"
                  , "l_quantity"
                  , "l_receiptdate"
                  , "l_sngflag"
                  , "l_shipdate"
                  , "l_shipinstruct"
                  , "l_shipmode"
                  , "l_suppkey"
                  , "l_tax"
                  ])
                  (TableHints (sng $ Key ("l_orderkey" :| ["l_linenumber"])) NonEmpty)

customers :: Q [Customer]
customers = table "customer"
                  ("c_acctbal" :|
                  [ "c_address"
                  , "c_comment"
                  , "c_custkey"
                  , "c_mktsegment"
                  , "c_name"
                  , "c_nationkey"
                  , "c_phone"
                  ])
                  (TableHints (sng $ Key (sng "c_custkey")) NonEmpty)



