{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Queries.TPCH.Q6
    ( q6
    ) where

import qualified Data.Time.Calendar as C
import           Database.DSH
import           Schema.TPCH

between :: Q Decimal -> Q Decimal -> Q Decimal -> Q Bool
between x l r = x >= l && x <= r

q6 :: Day -> Decimal -> Decimal -> Q Decimal
q6 startDate discount quantity =
  sum [ l_extendedpriceQ l * l_discountQ l
      | l <- lineitems
      , l_shipdateQ l >= toQ startDate
      , l_shipdateQ l < toQ (C.addDays 365 startDate)
      , between (l_discountQ l) lowerDiscount upperDiscount 
      , l_quantityQ l < toQ quantity
      ]
  where
    lowerDiscount = toQ discount - 0.01
    upperDiscount = toQ discount + 0.01
