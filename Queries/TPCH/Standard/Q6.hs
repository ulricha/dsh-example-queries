{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Queries.TPCH.Standard.Q6
    ( q6
    , q6Default
    ) where

import qualified Data.Decimal                as D
import qualified Data.Time.Calendar          as C
import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

-- | TPC-H Query Q6 with standard validation parameters
q6Default :: Q Decimal
q6Default = q6 (C.fromGregorian 1994 1 1)
               (D.realFracToDecimal 2 (0.06::Double)) 24

between :: Q Decimal -> Q Decimal -> Q Decimal -> Q Bool
between x l r = x >= l && x <= r

-- | TPC-H Query Q6
q6 :: Day -> Decimal -> Decimal -> Q Decimal
q6 startDate discount quantity =
  sum [ l_extendedpriceQ l * l_discountQ l
      | l <- lineitems
      , l_shipdateQ l `inInterval` yearInterval startDate 1
      , between (l_discountQ l) lowerDiscount upperDiscount
      , l_quantityQ l < toQ quantity
      ]
  where
    lowerDiscount = toQ discount - 0.01
    upperDiscount = toQ discount + 0.01
