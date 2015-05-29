{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | TPC-H Q19
module Queries.TPCH.Q19
    ( q19
    , q19Default
    , q19a
    , q19aDefault
    ) where

import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

between :: Q Integer -> Q Integer -> Q Integer -> Q Bool
between x l r = x >= l && x <= r

--------------------------------------------------------------------------------
-- Literal transcription of the TPC-H benchmark query

smPred :: Text -> Decimal -> Q Part -> Q LineItem -> Q Bool
smPred brand1 quantity1 p l =
  (p_partkeyQ p == l_partkeyQ l)
  && (p_brandQ p == toQ brand1)
  && (p_containerQ p `elem` toQ ["SM CASE", "SM BOX", "SM PACK", "SM PKG"])
  && (l_quantityQ l >= toQ quantity1)
  && (l_quantityQ l <= toQ quantity1 + 10)
  && (between (p_sizeQ p) 1 5)
  && (l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"])
  && (l_shipinstructQ l == "DELIVER IN PERSON")

medPred :: Text -> Decimal -> Q Part -> Q LineItem -> Q Bool
medPred brand2 quantity2 p l =
  p_partkeyQ p == l_partkeyQ l
  && p_brandQ p == toQ brand2
  && p_containerQ p `elem` toQ ["MED BAG", "MED BOX", "MED PKG", "MED PACK"]
  && l_quantityQ l >= toQ quantity2
  && l_quantityQ l<= toQ quantity2 + 10
  && between (p_sizeQ p) 1 10
  && l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"]
  && l_shipinstructQ l == "DELIVER IN PERSON"

lgPred :: Text -> Decimal -> Q Part -> Q LineItem -> Q Bool
lgPred brand3 quantity3 p l =
  p_partkeyQ p == l_partkeyQ l
  && p_brandQ p == toQ brand3
  && p_containerQ p `elem` toQ ["LG CASE", "LG BOX", "LG PACK", "LG PKG"]
  && l_quantityQ l >= toQ quantity3
  && l_quantityQ l<= toQ quantity3 + 10
  && between (p_sizeQ p) 1 15
  && l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"]
  && l_shipinstructQ l == "DELIVER IN PERSON"

-- | TPC-H Query Q19 with standard validation parameters (literal transcription)
q19aDefault :: Q Decimal
q19aDefault = q19a 1 10 20 "Brand#12" "Brand#23" "Brand#34"

-- | TPC-H Query Q19 (literal transcription)
q19a :: Decimal -> Decimal -> Decimal -> Text -> Text -> Text -> Q Decimal
q19a quantity1 quantity2 quantity3 brand1 brand2 brand3 =
  sum $
  [ l_extendedpriceQ l * (1 - l_discountQ l)
  | l <- lineitems
  , p <- parts
  , smPred brand1 quantity1 p l
    || medPred brand2 quantity2 p l
    || lgPred brand3 quantity3 p l
  ]

--------------------------------------------------------------------------------

smPred' :: Text -> Decimal -> Q Part -> Q LineItem -> Q Bool
smPred' brand1 quantity1 p l =
  (p_brandQ p == toQ brand1)
  && (p_containerQ p `elem` toQ ["SM CASE", "SM BOX", "SM PACK", "SM PKG"])
  && (l_quantityQ l >= toQ quantity1)
  && (l_quantityQ l <= toQ quantity1 + 10)
  && (between (p_sizeQ p) 1 5)
  && (l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"])
  && (l_shipinstructQ l == "DELIVER IN PERSON")

medPred' :: Text -> Decimal -> Q Part -> Q LineItem -> Q Bool
medPred' brand2 quantity2 p l =
  p_brandQ p == toQ brand2
  && p_containerQ p `elem` toQ ["MED BAG", "MED BOX", "MED PKG", "MED PACK"]
  && l_quantityQ l >= toQ quantity2
  && l_quantityQ l<= toQ quantity2 + 10
  && between (p_sizeQ p) 1 10
  && l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"]
  && l_shipinstructQ l == "DELIVER IN PERSON"

lgPred' :: Text -> Decimal -> Q Part -> Q LineItem -> Q Bool
lgPred' brand3 quantity3 p l =
  p_brandQ p == toQ brand3
  && p_containerQ p `elem` toQ ["LG CASE", "LG BOX", "LG PACK", "LG PKG"]
  && l_quantityQ l >= toQ quantity3
  && l_quantityQ l<= toQ quantity3 + 10
  && between (p_sizeQ p) 1 15
  && l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"]
  && l_shipinstructQ l == "DELIVER IN PERSON"

-- | TPC-H Query Q19 (alternative version) with standard validation
-- parameters
q19Default :: Q Decimal
q19Default = q19 1 10 20 "Brand#12" "Brand#23" "Brand#34"

-- | TPC-H Query Q19 (alternative version). The join argument has been
-- extracted from the branches of the disjunctive predicate
-- /manually/. Of course, doing this automatically is exactly the
-- point of the benchmark query.
q19 :: Decimal -> Decimal -> Decimal -> Text -> Text -> Text -> Q Decimal
q19 quantity1 quantity2 quantity3 brand1 brand2 brand3 =
  sum [ discPrice l
      | l <- lineitems
      , p <- parts
      , p_partkeyQ p == l_partkeyQ l
      , smPred' brand1 quantity1 p l
        || medPred' brand2 quantity2 p l
        || lgPred' brand3 quantity3 p l
      ]
