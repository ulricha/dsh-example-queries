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
    , q12'
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Queries.TPCH.Records

relevantShippings :: Text -> Text -> Integer -> Q [(Text, Text)]
relevantShippings sm1 sm2 date =
  [ pair (l_shipmodeQ l) (o_orderpriorityQ o)
  | o <- orders
  , l <- lineitems
  , o_orderkeyQ o == l_orderkeyQ l
  , l_shipmodeQ l `elem` toQ [sm1, sm2]
  , l_commitdateQ l < l_receiptdateQ l
  , l_shipdateQ l < l_commitdateQ l
  , l_receiptdateQ l >= toQ date
  , l_receiptdateQ l < (toQ date + 42)
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

q12 :: Text -> Text -> Integer -> Q [(Text, Integer, Integer)]
q12 sm1 sm2 date =
  [ tuple3 shipmode (highlineCount g) (lowlineCount g)
  | (view -> (shipmode, g)) <- groupWithKey fst (relevantShippings sm1 sm2 date)
  ]

  
-------------------------------------------------------------------------------
-- Alternative implementation of highline and lowline counts: exploit the
-- explicit representation of groups, which we may freely filter.
-- Question: is this beneficial?

lineCount :: (Q Text -> Q Bool) -> Q [(Text, Text)] -> Q Integer
lineCount opPred ops = length $ filter opPred $ map snd ops

q12' :: Text -> Text -> Integer -> Q [(Text, Integer, Integer)]
q12' sm1 sm2 date =
  [ tuple3 shipmode 
           (lineCount (\op -> op == "1-URGENT" || op == "2-HIGH") g)
	   (lineCount (\op -> op /= "1-URGENT" && op /= "2-HIGH") g)
  | (view -> (shipmode, g)) <- groupWithKey fst (relevantShippings sm1 sm2 date)
  ]
