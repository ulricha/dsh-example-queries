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

-- | TPC-H Q13: Distribution of orders per customer, including
-- customers without orders.
q13 :: Text -> Text -> Q [(Integer, Integer)]
q13 pat1 pat2 =
    reverse $ sortWith id $
    [ tup2 c_count (length g)
    | (view -> (c_count, g)) <- groupWithKey snd (ordersPerCustomer pat)
    ]
  where
    pat = T.append (T.cons '%' pat1) (T.cons '%' pat2)
