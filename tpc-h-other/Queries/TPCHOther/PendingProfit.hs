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

module Queries.TPCHOther.PendingProfit where

import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

expectedRevenueFor :: Text -> Q [(Text, [(Day, Decimal)])]
expectedRevenueFor nationName =
    [ pair (c_nameQ c) [ pair (o_orderdateQ o) (orderRevenue o)
                       | o <- ordersWithStatus "P" c ]
    | c <- customers
    , c `hasNationality` nationName
    , or [ toQ True | _ <- ordersWithStatus "P" c ]
    ]
