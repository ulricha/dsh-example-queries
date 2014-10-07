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

module Queries.TPCH.Q18
    ( q18
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

largeOrders :: Double -> Q [Integer]
largeOrders quantity = 
  [ ok
  | (view -> (ok, ls)) <- groupWithKey l_orderkeyQ lineitems
  , toQ quantity < sum (map l_quantityQ ls)
  ]

sortSpec :: Q ((Text, Integer, Integer, Integer, Double), Double) -> Q (Double, Integer)
sortSpec gs =
  let (view -> (_, _, _, orderDate, totalPrice)) = fst gs
  in pair (-1 * totalPrice) orderDate

q18 :: Double -> Q [((Text, Integer, Integer, Integer, Double), Double)]
q18 quantity =
  sortWith sortSpec $
  map (\(view -> (k, g)) -> pair k (sum $ map snd g)) $
  groupWithKey fst $
  [ pair (tup5 (c_nameQ c) (c_custkeyQ c) (o_orderkeyQ o) (o_orderdateQ o) (o_totalpriceQ o))
         (l_quantityQ l)
  | c <- customers
  , o <- orders
  , l <- lineitems
  , c_custkeyQ c == o_custkeyQ o
  , o_orderkeyQ o == l_orderkeyQ l
  , o_orderkeyQ o `elem` largeOrders quantity
  ]
