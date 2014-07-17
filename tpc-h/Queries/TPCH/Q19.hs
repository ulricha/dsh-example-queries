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
    
-- TPC-H Q19

module Queries.TPCH.Q19
    ( q19
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

between :: Q Integer -> Q Integer -> Q Integer -> Q Bool
between x l r = x >= l && x <= r

-- Literal transcription of the TPC-H benchmark query
smPred :: Text -> Double -> Q Part -> Q LineItem -> Q Bool
smPred brand1 quantity1 p l = 
  (p_partkeyQ p == l_partkeyQ l)
  && (p_brandQ p == toQ brand1)
  && (p_containerQ p `elem` toQ ["SM CASE", "SM BOX", "SM PACK", "SM PKG"])
  && (l_quantityQ l >= toQ quantity1)
  && (l_quantityQ l <= toQ quantity1 + 10)
  && (between (p_sizeQ p) 1 5)
  && (l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"])
  && (l_shipinstructQ l == "DELIVER IN PERSON")

medPred :: Text -> Double -> Q Part -> Q LineItem -> Q Bool
medPred brand2 quantity2 p l = 
  p_partkeyQ p == l_partkeyQ l
  && p_brandQ p == toQ brand2
  && p_containerQ p `elem` toQ ["MED BAG", "MED BOX", "MED PKG", "MED PACK"]
  && l_quantityQ l >= toQ quantity2
  && l_quantityQ l<= toQ quantity2 + 10
  && between (p_sizeQ p) 1 10
  && l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"]
  && l_shipinstructQ l == "DELIVER IN PERSON"

lgPred :: Text -> Double -> Q Part -> Q LineItem -> Q Bool
lgPred brand3 quantity3 p l = 
  p_partkeyQ p == l_partkeyQ l
  && p_brandQ p == toQ brand3
  && p_containerQ p `elem` toQ ["LG CASE", "LG BOX", "LG PACK", "LG PKG"]
  && l_quantityQ l >= toQ quantity3
  && l_quantityQ l<= toQ quantity3 + 10
  && between (p_sizeQ p) 1 15
  && l_shipmodeQ l `elem` toQ ["AIR", "AIR REG"]
  && l_shipinstructQ l == "DELIVER IN PERSON"
  

q19 :: Double -> Double -> Double -> Text -> Text -> Text -> Q Double
q19 quantity1 quantity2 quantity3 brand1 brand2 brand3 =
  sum $
  [ l_extendedpriceQ l * (1 - l_discountQ l)
  | l <- lineitems
  , p <- parts
  , smPred brand1 quantity1 p l
    || medPred brand2 quantity2 p l
    || lgPred brand3 quantity3 p l
  ]
