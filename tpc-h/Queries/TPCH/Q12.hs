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

-- TPC-H Q12

module Queries.TPCH.Q12
    ( q12
    , q12a
    , q12Default
    , q12aDefault
    ) where

import qualified Data.Time.Calendar as C
import           Database.DSH
import           Schema.TPCH

relevantShippings :: Text -> Text -> Day -> Q [(Text, Text)]
relevantShippings sm1 sm2 date =
  [ pair (l_shipmodeQ l) (o_orderpriorityQ o)
  | o <- orders
  , l <- lineitems
  , o_orderkeyQ o == l_orderkeyQ l
  , l_shipmodeQ l `elem` toQ [sm1, sm2]
  , l_commitdateQ l < l_receiptdateQ l
  , l_shipdateQ l < l_commitdateQ l
  , l_receiptdateQ l >= toQ date
  , l_receiptdateQ l < toQ (C.addDays 365 date)
  ]

-------------------------------------------------------------------------------
-- Literal transcription of the TPC-H benchmark query
-- Note: Due to SQL's restrictions, the computation of highline and lowline counts
-- looks rather awkward.

highlineCount :: Q [(Text, Text)] -> Q Integer
highlineCount ops =
  sum [ if op == "1-URGENT" || op == "2-HIGH"
        then 1
        else 0
      | op <- map snd ops
      ]

lowlineCount :: Q [(Text, Text)] -> Q Integer
lowlineCount ops =
  sum [ if op /= "1-URGENT" && op /= "2-HIGH"
        then 1
        else 0
      | op <- map snd ops
      ]

-- | TPC-H Query Q12 with standard validation parameters
q12Default :: Q [(Text, Integer, Integer)]
q12Default = q12 "MAIL" "SHIP" (C.fromGregorian 1994 1 1)

-- | TPC-H Query Q12
q12 :: Text -> Text -> Day -> Q [(Text, Integer, Integer)]
q12 sm1 sm2 date =
  [ tup3 shipmode (highlineCount g) (lowlineCount g)
  | (view -> (shipmode, g)) <- groupWithKey fst (relevantShippings sm1 sm2 date)
  ]

-------------------------------------------------------------------------------

lineCount :: (Q Text -> Q Bool) -> Q [(Text, Text)] -> Q Integer
lineCount opPred ops = length $ filter opPred $ map snd ops

q12aDefault :: Q [(Text, Integer, Integer)]
q12aDefault = q12a "MAIL" "SHIP" (C.fromGregorian 1994 1 1)

-- | Alternative implementation of highline and lowline counts: exploit the
-- explicit representation of groups, which we may freely filter.
-- Question: is this beneficial?
q12a :: Text -> Text -> Day -> Q [(Text, Integer, Integer)]
q12a sm1 sm2 date =
  [ tup3 shipmode
         (lineCount (\op -> op == "1-URGENT" || op == "2-HIGH") g)
         (lineCount (\op -> op /= "1-URGENT" && op /= "2-HIGH") g)
  | (view -> (shipmode, g)) <- groupWithKey fst (relevantShippings sm1 sm2 date)
  ]
