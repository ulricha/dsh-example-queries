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

module Queries.TPCH.Q6
    ( q6
    ) where

import Database.DSH
import Schema.TPCH

between :: Q Decimal -> Q Decimal -> Q Decimal -> Q Bool
between x l r = x >= l && x <= r

q6 :: Q Decimal
q6 =
  sum $
  [ l_extendedpriceQ l * l_discountQ l
  | l <- lineitems
  , l_shipdateQ l >= 23
  , l_shipdateQ l < 42
  , between (l_discountQ l) ((toQ 0.05) - (toQ 0.01)) ((toQ 0.05) + (toQ 0.01))
  , l_quantityQ l < 24
  ]
