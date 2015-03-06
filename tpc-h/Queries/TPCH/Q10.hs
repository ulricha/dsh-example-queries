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

-- TPC-H Q10

module Queries.TPCH.Q10
    ( q10
    ) where

import qualified Data.Time.Calendar as C
import           Database.DSH
import           Schema.TPCH

q10_join :: Day -> Q [((Integer, Text, Decimal, Text, Text, Text, Text), Decimal)]
q10_join startDate =
  [ pair (tup7 (c_custkeyQ c)
               (c_nameQ c)
               (c_acctbalQ c)
               (c_phoneQ c)
               (n_nameQ n)
               (c_addressQ c)
               (c_commentQ c))
         (l_extendedpriceQ l * (1 - l_discountQ l))
  | c <- customers
  , o <- orders
  , l <- lineitems
  , n <- nations
  , c_custkeyQ c == o_custkeyQ o
  , l_orderkeyQ l == o_orderkeyQ o
  , o_orderdateQ o >= toQ startDate
  , o_orderdateQ o < toQ (C.addDays 90 startDate)
  , l_returnflagQ l == "R"
  , c_nationkeyQ c == n_nationkeyQ n
  ]

q10 :: Day -> Q [((Integer, Text, Decimal, Text, Text, Text, Text), Decimal)]
q10 startDate =
  sortWith snd $
  [ pair k (sum $ map snd g)
  | (view -> (k, g)) <- groupWithKey fst (q10_join startDate)
  ]
