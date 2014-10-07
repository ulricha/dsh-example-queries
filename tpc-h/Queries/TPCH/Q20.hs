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
    
-- TPC-H Q20

module Queries.TPCH.Q20
    ( q20
    ) where

import qualified Data.Text as T

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

data Interval = Interval { iv_start :: Integer, iv_end :: Integer }

inInterval :: Q Integer -> Interval -> Q Bool
inInterval d interval = d >= toQ (iv_start interval) && d < toQ (iv_end interval)


-- | Only consider parts of a given color
colorParts :: Text -> Q [Integer]
colorParts color = [ p_partkeyQ p | p <- parts, p_nameQ p `like` (toQ $ T.append color "%") ]

-- | Having more than 50% of the volume sold in a given time interval
-- in stock for a given part is considered excessive.
excessBoundary :: Interval -> Q Integer -> Q Double
excessBoundary interval partkey =
  0.5 * sum [ l_quantityQ l
            | l <- lineitems
	    , l_partkeyQ l == partkey
            , l_shipdateQ l `inInterval` interval
	    ]

-- | Compute suppliers who have an excess stock for parts of a given
-- color.
excessSuppliers :: Text -> Interval -> Q [Integer]
excessSuppliers color interval =
  [ ps_suppkeyQ ps
  | ps <- partsupps
  , ps_partkeyQ ps `elem` colorParts color
  , integerToDouble (ps_availqtyQ ps) > excessBoundary interval (ps_partkeyQ ps)
  ]

-- | Compute suppliers in a given nation who have an excess stock for
-- parts of a given color.
q20 :: Text -> Interval -> Text -> Q [(Text, Text)]
q20 color interval nation = 
  [ pair (s_nameQ s) (s_addressQ s)
  | s <- suppliers
  , n <- nations
  , s_suppkeyQ s `elem` excessSuppliers color interval
  , s_nationkeyQ s == n_nationkeyQ n
  , n_nameQ n == toQ nation
  ]
