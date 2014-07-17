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
    
-- TPC-H Q4

module Queries.TPCH.Q4
    ( q4
    , q4'
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

q4 =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g)) 
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= 42
    , o_orderdateQ o < 42 + 57
    , not $ null [ toQ ()
                 | l <- lineitems
                 , l_orderkeyQ l == o_orderkeyQ o
                 , l_commitdateQ l < l_receiptdateQ l
                 ]
    ]

q4' =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g)) 
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= 42
    , o_orderdateQ o < 42 + 57
    , any (\l -> l_commitdateQ l < l_receiptdateQ l 
                 && l_orderkeyQ l == o_orderkeyQ o) 
          lineitems
    ]
