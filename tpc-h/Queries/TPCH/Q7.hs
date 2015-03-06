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

-- TPC-H Q7

module Queries.TPCH.Q7
    ( q7
    ) where

import qualified Data.Time.Calendar  as C
import           Database.DSH
import           Queries.TPCH.Common
import           Schema.TPCH

q7 :: Text -> Text -> Q [((Text, Text, Integer), Decimal)]
q7 nation1 nation2 =
  sortWith fst $
  map (\(view -> (k, g)) -> pair k (sum $ [ v | (view -> (_, _, _, v)) <- g])) $
  groupWithKey (\(view -> (n1, n2, y, _)) -> tup3 n1 n2 y) $
  [ xs
  | xs <- [ tup4 (n_nameQ n1)
                 (n_nameQ n2)
                 (dateYear $ l_shipdateQ l)
                 (l_extendedpriceQ l * (1 - l_discountQ l))
          | s <- suppliers
          , l <- lineitems
          , o <- orders
          , c <- customers
          , n1 <- nations
          , n2 <- nations
          , s_suppkeyQ s == l_suppkeyQ l
          , o_orderkeyQ o == l_orderkeyQ l
          , c_custkeyQ c == o_custkeyQ o
          , s_nationkeyQ s == n_nationkeyQ n1
          , c_nationkeyQ c == n_nationkeyQ n2
          , (n_nameQ n1 == toQ nation1 && n_nameQ n2 == toQ nation2)
            || (n_nameQ n1 == toQ nation2 && n_nameQ n2 == toQ nation1)
          , inInterval (l_shipdateQ l) interval
          ]
  ]

  where
    interval = Interval (C.fromGregorian 1995 1 1) (C.fromGregorian 1996 12 31)
