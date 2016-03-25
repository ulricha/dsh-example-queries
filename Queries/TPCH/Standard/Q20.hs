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

-- | TPC-H Q20
module Queries.TPCH.Standard.Q20
    ( q20
    , q20Default
    ) where

import qualified Data.Text as T
import qualified Data.Time.Calendar as C
import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks

-- | Only consider parts of a given color
colorParts :: Text -> Q [Integer]
colorParts color = [ p_partkeyQ p
                   | p <- parts
                   , p_nameQ p `like` (toQ $ T.append color "%")
                   ]

-- | Quantities sold for a given part by a given supplier in a year.
stockQuantities :: Interval -> Q PartSupp -> Q [Decimal]
stockQuantities interval ps =
    [ l_quantityQ l
    | l <- lineitems
    , l_partkeyQ l == ps_partkeyQ ps
    , l_suppkeyQ l == ps_suppkeyQ ps
    , l_shipdateQ l `inInterval` interval
    ]

-- | Having more than 50% of the volume sold in a given time interval
-- in stock for a given part is considered excessive.
excessBoundary :: Interval -> Q PartSupp -> Q Decimal
excessBoundary interval ps =
    0.5 * sum (stockQuantities interval ps)

-- | Compute suppliers who have an excess stock for parts of a given
-- color.
excessSuppliers :: Text -> Interval -> Q [Integer]
excessSuppliers color interval =
    [ ps_suppkeyQ ps
    | ps <- partsupps
    , ps_partkeyQ ps `elem` colorParts color
    -- The SQL version of Q20 implicitly selects only those suppliers that do
    -- actually ship the part in the sepcified interval. For suppliers which
    -- don't supply in this interval, the correlated subquery will return NULL
    -- (SUM returns NULL on an empty input) and the predicate will fail. For the
    -- DSH 'sum' aggregate, we take care to preserve the (reasonable) behaviour
    -- of the regular Haskell 'sum' combinator: For an empty input, we get 0.
    -- However, for Q20 this means that the predicate will evaluate to true for
    -- suppliers which do not supply in the interval. To simulate the SQL
    -- behaviour, we explicitly exclude all suppliers that do not supply.
    , not $ null $ stockQuantities interval ps
    , integerToDecimal (ps_availqtyQ ps) > excessBoundary interval ps
    ]

-- | TPC-H Query Q20 with standard validation parameters
q20Default :: Q [(Text, Text)]
q20Default = q20 "forest" (C.fromGregorian 1994 1 1) "CANADA"

-- | TPC-H Query Q20: Compute suppliers in a given nation who have an
-- excess stock for parts of a given color.
q20 :: Text -> Day -> Text -> Q [(Text, Text)]
q20 color startDate nationName =
    [ pair (s_nameQ s) (s_addressQ s)
    | s <- suppliers
    , n <- nations
    , s_suppkeyQ s `elem` excessSuppliers color interval
    , s_nationkeyQ s == n_nationkeyQ n
    , n_nameQ n == toQ nationName
    ]
  where
    interval = yearInterval startDate 1
