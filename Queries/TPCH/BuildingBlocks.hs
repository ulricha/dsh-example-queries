{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}

-- | Common building blocks for TPC-H queries
module Queries.TPCH.BuildingBlocks
    ( -- * Date intervals
      Interval(..)
    , inInterval
    , intervalFrom
      -- * Various predicates and filters
    , custFromNation
    , supplierFromNation
    , ordersWithStatus
    , customersFrom
    , orderedBy
    , fromRegion
      -- * One-to-many relationships along foreign keys
    , custOrders
    , orderItems
    , partSuppliers
    , supplierParts
    , regionNations
    , supplierItems
      -- * Price and revenue
    , discPrice
    , chargedPrice
    , revenue
    , orderRevenue
    ) where


import Database.DSH
import Schema.TPCH
import qualified Data.Time.Calendar as C

--------------------------------------------------------------------------------
-- Date intervals

data Interval = Interval { iv_start :: Day, iv_end :: Day }

inInterval :: Q Day -> Interval -> Q Bool
inInterval d interval = d >= toQ (iv_start interval) && d < toQ (iv_end interval)

intervalFrom :: Day -> Integer -> Interval
intervalFrom d len = Interval d (C.addDays len d)

--------------------------------------------------------------------------------
-- Various predicates and filters

-- | Return all orders with a given status
ordersWithStatus :: Text -> Q [Order]
ordersWithStatus status =
    [ o | o <- orders, o_orderstatusQ o == toQ status ]

-- | Does the customer originate from the given nation?
custFromNation :: Q Customer -> Text -> Q Bool
custFromNation c nn =
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == c_nationkeyQ c
       | n <- nations
       ]

-- | Does the supplier originate from the given nation?
supplierFromNation :: Text -> Q Supplier -> Q Bool
supplierFromNation nn s =
    or [ n_nameQ n == toQ nn && n_nationkeyQ n == s_nationkeyQ s
       | n <- nations
       ]

-- | Return all customers from a list of (phone) country codes
customersFrom :: [Text] -> Q [Customer]
customersFrom areaPrefixes =
    filter (\c -> subString 1 2 (c_phoneQ c) `elem` toQ areaPrefixes)
           customers

-- | Has the order been ordered by a given customer?
orderedBy :: Q Order -> Q Customer -> Q Bool
orderedBy o c = o_custkeyQ o == c_custkeyQ c

-- | Check wether the given nation key lies in the given region.
fromRegion :: Q Integer -> Text -> Q Bool
fromRegion nationKey regionName =
    or [ n_nationkeyQ n == nationKey
       | r <- regions
       , r_nameQ r == toQ regionName
       , n <- regionNations r
       ]

--------------------------------------------------------------------------------
-- One-to-many relationships along foreign keys

-- | All lineitems of one particular order
orderItems :: Q Order -> Q [LineItem]
orderItems o = [ l | l <- lineitems, l_orderkeyQ l == o_orderkeyQ o ]

-- | All orders of one particular customer
custOrders :: Q Customer -> Q [Order]
custOrders c = [ o | o <- orders, o_custkeyQ o == c_custkeyQ c ]

-- | Which supplier supplies the specified part?
partSuppliers :: Q Part -> Q [(Supplier, PartSupp)]
partSuppliers p = [ tup2 s ps
                  | s <- suppliers
                  , ps <- partsupps
                  , ps_partkeyQ ps == p_partkeyQ p
                  , ps_suppkeyQ ps == s_suppkeyQ s
                  ]

supplierParts :: Q Supplier -> Q [(Part, PartSupp)]
supplierParts s = [ tup2 p ps
                  | ps <- partsupps
                  , p <- parts
                  , s_suppkeyQ s == ps_suppkeyQ ps
                  , ps_partkeyQ ps == p_partkeyQ p
                  ]

-- | All nations in a region.
regionNations :: Q Region -> Q [Nation]
regionNations r = [ n | n <- nations, n_regionkeyQ n == r_regionkeyQ r ]

-- | All items for which the given supplier was chosen.
supplierItems :: Q Supplier -> Q [LineItem]
supplierItems s = [ l | l <- lineitems, l_suppkeyQ l == s_suppkeyQ s ]

--------------------------------------------------------------------------------
-- Price and revenue

-- | The discounted price of an item
discPrice :: Q LineItem -> Q Decimal
discPrice l = l_extendedpriceQ l * (1 - l_discountQ l)

-- | The price of an item after taxes
chargedPrice :: Q LineItem -> Q Decimal
chargedPrice l = discPrice l * (1 + l_taxQ l)

-- | The total price of a number of items.
revenue :: Q [LineItem] -> Q Decimal
revenue ls = sum $ map discPrice ls

-- | Revenue from a given order.
orderRevenue :: Q Order -> Q Decimal
orderRevenue o = revenue $ orderItems o

