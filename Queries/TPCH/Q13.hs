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

-- TPC-H Q13

module Queries.TPCH.Q13
    ( q13
    , q13Default
    ) where

import qualified Data.Text as T

import Database.DSH
import Schema.TPCH

-- TPC-H Q13. Note that we replace the LEFT OUTER JOIN and grouping
-- with a nestjoin, to include those customers with an empty list of
-- (relevant) orders.

-- | Compute all orders for a given customer that do not fall into
-- certain categories.
custOrders :: Text -> Q Customer -> Q [Order]
custOrders pat c = [ o
                   | o <- orders
                   , c_custkeyQ c == o_custkeyQ o
                   , not $ o_commentQ o `like` (toQ pat)
                   ]

-- | Compute number of orders per customer, including those that have
-- not placed any orders.
ordersPerCustomer :: Text -> Q [(Integer, Integer)]
ordersPerCustomer pat =
    [ tup2 (c_custkeyQ c) (length $ (custOrders pat c))
    | c <- customers
    ]

-- | TPC-H Query Q13 with standard validation parameters
q13Default :: Q [(Integer, Integer)]
q13Default = q13 "special" "requests"

-- | TPC-H Q13: Distribution of orders per customer, including
-- customers without orders.
q13 :: Text -> Text -> Q [(Integer, Integer)]
q13 w1 w2 =
    reverse $ sortWith id $ groupAggr snd id length (ordersPerCustomer pat)
  where
    pat = T.append (T.cons '%' w1) (T.cons '%' w2)
