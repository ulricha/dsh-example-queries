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

-- TPC-H Q9

module Queries.TPCH.Q9
    ( q9
    , q9Default
    ) where

import qualified Data.Text                    as T
import           Database.DSH
import           Schema.TPCH
import           Queries.TPCH.BuildingBlocks

thd3 :: (QA a, QA b, QA c) => Q (a, b, c) -> Q c
thd3 (view -> (_, _, c)) = c

profit :: Text -> Q [(Text, Integer, Decimal)]
profit color =
    [ tup3 (n_nameQ n)
           (dateYear $ o_orderdateQ o)
           (discPrice l - ps_supplycostQ ps * l_quantityQ l)
    | p <- parts
    , s <- suppliers
    , l <- lineitems
    , ps <- partsupps
    , o <- orders
    , n <- nations
    , s_suppkeyQ s == l_suppkeyQ l
    , ps_suppkeyQ ps == l_suppkeyQ l
    , ps_partkeyQ ps == l_partkeyQ l
    , p_partkeyQ p == l_partkeyQ l
    , o_orderkeyQ o == l_orderkeyQ l
    , s_nationkeyQ s == n_nationkeyQ n
    , p_nameQ p `like` (toQ "%green%")
    ]
  where
    colorPattern = T.append (T.append "%" color) "%"

-- | TPC-H Query Q9 with standard validation parameters
q9Default :: Q [(Text, Integer, Decimal)]
q9Default = q9 "green"

-- | TPC-H Query Q9
q9 :: Text -> Q [(Text, Integer, Decimal)]
q9 color =
   sortWith (\(view -> (n, y, _)) -> pair n (y * (-1)))
   [ tup3 (fst k) (snd k) (sum $ map thd3 g)
   | (view -> (k, g)) <- groupWithKey (\(view -> (n, y, _)) -> pair n y)
                                      (profit color)
   ]
