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

-- | TPC-H Q13
module Queries.TPCH.Q13
    ( q13
    , q13Default
    ) where

import qualified Data.Text as T

import Database.DSH
import Queries.TPCH.BuildingBlocks
import Schema.TPCH

-- TPC-H Q13. Note that we replace the LEFT OUTER JOIN and grouping
-- with a nestjoin, to include those customers with an empty list of
-- (relevant) orders.

-- | Compute number of orders per customer, including those that have
-- not placed any orders.
ordersPerCustomer :: Text -> Q [(Integer, Integer)]
ordersPerCustomer pat =
    [ tup2 (c_custkeyQ c)
           (length $ filter (\o -> o_commentQ o `notLike` (toQ pat))
                   $ custOrders c)
    | c <- customers
    ]

-- | TPC-H Query Q13 with standard validation parameters
q13Default :: Q [(Integer, Integer)]
q13Default = q13 "special" "requests"

-- | TPC-H Q13: Distribution of orders per customer, including
-- customers without orders.
q13 :: Text -> Text -> Q [(Integer, Integer)]
q13 w1 w2 =
    sortWith (\(view -> (c_count, custdist)) -> pair (custdist * (-1))
                                                     (c_count * (-1)))
    $ groupAggr snd id length (ordersPerCustomer pat)
  where
    pat = foldr T.append "" ["%", w1, "%", w2, "%"]
