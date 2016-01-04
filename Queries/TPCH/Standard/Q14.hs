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

-- | TPC-H Q14
module Queries.TPCH.Standard.Q14
    ( q14
    , q14a
    , q14Default
    , q14aDefault
    ) where

import qualified Data.Time.Calendar as C
import Database.DSH
import Schema.TPCH
import Queries.TPCH.BuildingBlocks


-------------------------------------------------------------------------------

-- | TPC-H Query Q14 with standard validation parameters
q14Default :: Q Decimal
q14Default = q14 (C.fromGregorian 1995 9 1)

-- | TPC-H Query Q14 with standard validation parameters (alternative
-- formulation)
q14aDefault :: Q Decimal
q14aDefault = q14a (C.fromGregorian 1995 9 1)

-------------------------------------------------------------------------------

itemPrices :: Day -> Q [(Text, LineItem)]
itemPrices startDate =
  [ tup2 (p_typeQ p) l
  | l <- lineitems
  , p <- parts
  , l_partkeyQ l == p_partkeyQ p
  , inInterval (l_shipdateQ l) (monthInterval startDate 1)
  ]

-------------------------------------------------------------------------------
-- Literal transcription of the TPC-H benchmark query

q14 :: Day -> Q Decimal
q14 startDate = 100.0 * promoRev / totalRev
  where

    promoRev = sum [ if ty `like` "PROMO%"
                     then discPrice l
                     else 0
                   | (view -> (ty, l)) <- itemPrices startDate
                   ]

    totalRev = revenue $ map snd $ itemPrices startDate

-------------------------------------------------------------------------------
-- Variation which uses a subquery to reduce the number of tuples that go
-- into the aggregate. This formulation might be beneficial if the p_type 
-- predicate can be pushed to an index.

q14a :: Day -> Q Decimal
q14a startDate = 100.0 * promoRev / totalRev
  where
    promoRev = sum [ discPrice l
                   | (view -> (ty, l)) <- itemPrices startDate
                   , ty `like` "PROMO%"
                   ]

    totalRev = revenue $ map snd $ itemPrices startDate
