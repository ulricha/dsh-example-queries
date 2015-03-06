{-# LANGUAGE ViewPatterns #-}

module Queries.TPCH.Common where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler
import Schema.TPCH

data Interval = Interval { iv_start :: Day, iv_end :: Day }

inInterval :: Q Day -> Interval -> Q Bool
inInterval d interval = d >= toQ (iv_start interval) && d < toQ (iv_end interval)

revenue :: Q LineItem -> Q Decimal
revenue l = l_extendedpriceQ l * (1 - l_discountQ l)

dateYear :: Q Day -> Q Integer
dateYear d = let (view -> (year, _, _)) = toGregorian d
             in year
