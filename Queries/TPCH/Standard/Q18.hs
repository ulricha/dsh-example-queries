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

-- TPC-H Q18

module Queries.TPCH.Standard.Q18
    ( q18Default
    , q18
    ) where

import Database.DSH
import Schema.TPCH

largeOrders :: Decimal -> Q [Integer]
largeOrders quantity =
  [ ok
  | (view -> (ok, ls)) <- groupWithKey l_orderkeyQ lineitems
  , toQ quantity < sum (map l_quantityQ ls)
  ]

sortSpec :: Q ((Text, Integer, Integer, Day, Decimal), Decimal) -> Q (Decimal, Day)
sortSpec gs =
  let (view -> (_, _, _, orderDate, totalPrice)) = fst gs
  in pair (-1 * totalPrice) orderDate

-- | TPC-H Query Q18 with standard validation parameters
q18Default :: Q [((Text, Integer, Integer, Day, Decimal), Decimal)]
q18Default = q18 300

-- | TPC-H Query Q18
q18 :: Decimal -> Q [((Text, Integer, Integer, Day, Decimal), Decimal)]
q18 quantity =
  take 100 $
  sortWith sortSpec $
  groupAggr fst snd sum
  [ pair (tup5 (c_nameQ c) (c_custkeyQ c) (o_orderkeyQ o) (o_orderdateQ o) (o_totalpriceQ o))
         (l_quantityQ l)
  | c <- customers
  , o <- orders
  , l <- lineitems
  , c_custkeyQ c == o_custkeyQ o
  , o_orderkeyQ o == l_orderkeyQ l
  , o_orderkeyQ o `elem` largeOrders quantity
  ]
