{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE OverloadedStrings   #-}

module Queries.TPCHOther.TopK where

import qualified Prelude as P
import Database.DSH
import Schema.TPCH

import Queries.TPCH.BuildingBlocks

topKOrders :: Integer -> Q [Order] -> Q [Order]
topKOrders k os = take (toQ k)
                  $ map fst
                  $ sortWith snd
                  $ [ tup2 o (length $ orderItems o) | o <- os ]


-- | Task: For a given set of customers, compute the k orders with the
-- most lineitems.
q :: Integer -> Q [(Text, [Order])]
q k = [ tup2 (c_nameQ c) (topKOrders k (custOrders c))
      | c <- customers
      , c `hasNationality` "GERMANY"
      ]
