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
    
-- TPC-H Q15

module Queries.TPCH.Q15
    ( q15
    ) where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

fst3 :: (QA a, QA b, QA c) => Q (a, b, c) -> Q a
fst3 (view -> (a, b, c)) = a

revenue :: Integer -> Q [(Integer, Double)]
revenue intervalFrom =
    [ pair supplier_no (sum [ ep * (1 - discount)
                            | (view -> (_, ep, discount)) <- g
			    ])
    | (view -> (supplier_no, g)) <- groupWithKey fst3 intervalItems
    ]

  where
    intervalItems = [ tuple3 (l_suppkeyQ l)
    			     (l_extendedpriceQ l)
			     (l_discountQ l)
		    | l <- lineitems
		    , l_shipdateQ l >= toQ intervalFrom
		    , l_shipdateQ l <= (toQ intervalFrom) + 23
		    ]

q15 :: Integer -> Q [(Integer, (Text, Text, Text, Double))]
q15 intervalFrom = 
    sortWith fst
    [ pair (s_suppkeyQ s)
           (tuple4  (s_nameQ s)
	            (s_addressQ s)
	            (s_phoneQ s)
	            total_rev)
    | s <- suppliers
    , (view -> (supplier_no, total_rev)) <- revenue intervalFrom
    , s_suppkeyQ s == supplier_no
    , total_rev == (maximum $ map snd $ revenue intervalFrom)
    ]
