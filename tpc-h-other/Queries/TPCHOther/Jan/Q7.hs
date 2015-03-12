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

module Queries.TPCHOther.Jan.Q7 where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

--------------------------------------------------------------------------------
-- Query written from a viewpoint taking the list model in consideration

orderQuantity :: Q [LineItem] -> Q Decimal
orderQuantity lis = sum $ map l_quantityQ lis

jan_q7a :: Q [LineItem]
jan_q7a = snd $ head $ sortWith (orderQuantity . snd) $ groupWithKey l_orderkeyQ lineitems

--------------------------------------------------------------------------------
-- Query written from a database viewpoint

-- List the lineitems of the order with the most parts.
sumPerOrder :: Q [(Integer, Decimal)]
sumPerOrder = map (\(view -> (ok, lis)) -> pair ok (sum $ map l_quantityQ lis))
              $ groupWithKey l_orderkeyQ lineitems

jan_q7b :: Q [LineItem]
jan_q7b =
    [ l
    | l <- lineitems
    , (view -> (ok, nrItems)) <- sumPerOrder
    , l_orderkeyQ l == ok
    , nrItems == maximum(map snd sumPerOrder)
    ]
