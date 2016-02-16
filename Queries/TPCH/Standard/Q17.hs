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

-- | TPC-H Q17
module Queries.TPCH.Standard.Q17
    ( q17
    , q17Default
    ) where

import Database.DSH
import Schema.TPCH

-- | TPC-H Query Q17 with standard validation parameters
q17Default :: Q Decimal
q17Default = q17 "Brand#23" "MED BOX"

-- | TPC-H Query Q17
q17 :: Text -> Text -> Q Decimal
q17 brand container =
    let prices = [ l_extendedpriceQ l
                 | l <- lineitems
                 , p <- parts
                 , p_partkeyQ p == l_partkeyQ l
                 , p_brandQ p == toQ brand
                 , p_containerQ p == toQ container
                 , l_quantityQ l < 0.2 * avg [ l_quantityQ l2
                                             | l2 <- lineitems
                                             , l_partkeyQ l2 == p_partkeyQ p
                                             ]
                 ]
    in sum prices / 7.0
