{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

-- | TPC-H Q7
module Queries.TPCH.Standard.Q7
    ( q7
    , q7a
    , q7Default
    , q7aDefault
    ) where

import qualified Data.Time.Calendar  as C
import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

-- | TPC-H Query Q7 with standard validation parameters
q7Default :: Q [((Text, Text, Integer), Decimal)]
q7Default = q7 "FRANCE" "GERMANY"

-- | TPC-H Query Q7 (alternative formulation) with standard validation
-- parameters
q7aDefault :: Q [((Text, Text, Integer), Decimal)]
q7aDefault = q7 "FRANCE" "GERMANY"

--------------------------------------------------------------------------------

-- | TPC-H Query Q7
q7 :: Text -> Text -> Q [((Text, Text, Integer), Decimal)]
q7 nation1 nation2 = sortWith fst $ totalRevenueBetweenNations nation1 nation2

nationPair :: Text -> Text -> Q Nation -> Q Nation -> Q Bool
nationPair nationName1 nationName2 n1 n2 =
    (n_nameQ n1 == toQ nationName1 && n_nameQ n2 == toQ nationName2)
    || (n_nameQ n1 == toQ nationName2 && n_nameQ n2 == toQ nationName1)

totalRevenueBetweenNations :: Text -> Text -> Q [((Text, Text, Integer), Decimal)]
totalRevenueBetweenNations nationName1 nationName2 =
    groupAggr fst snd sum (revenueBetweenNations nationName1 nationName2)

-- | For all items traded between given pairs of nations in a time
-- interval compute the revenue.
revenueBetweenNations :: Text -> Text -> Q [((Text, Text, Integer), Decimal)]
revenueBetweenNations nationName1 nationName2 =
    [ tup2 (tup3 n1 n2 (dateYear $ l_shipdateQ l)) (discPrice l)
    | (view -> (_, l, n1, n2)) <- binationalOrderItems nationName1 nationName2
    , l_shipdateQ l >= toQ (C.fromGregorian 1995 1 1)
    , l_shipdateQ l <= toQ (C.fromGregorian 1996 12 31)
    ]

-- | Compute suppliers of orders where the customer is located in a
-- given country and the part supplier is located in another given
-- country.
binationalOrderItems :: Text -> Text -> Q [(Supplier, LineItem, Text, Text)]
binationalOrderItems nationName1 nationName2 =
    [ tup4 s l (n_nameQ n1) (n_nameQ n2)
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
    , nationPair nationName1 nationName2 n1 n2
    ]

--------------------------------------------------------------------------------

-- | A rather literal transcription of TPC-H Q7
q7a :: Text -> Text -> Q [((Text, Text, Integer), Decimal)]
q7a nation1 nation2 =
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
