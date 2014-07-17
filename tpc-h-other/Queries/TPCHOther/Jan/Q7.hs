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

module Queries.TPCHOther.Jan.Q7 where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Schema.TPCH

-- List the lineitems of the order with the most parts.
sumPerOrder :: Q [(Integer, Double)]
sumPerOrder = map (\(view -> (ok, lis)) -> pair ok (sum $ map l_quantityQ lis)) 
	      $ groupWithKey l_orderkeyQ lineitems

jan_q7 :: Q [LineItem]
jan_q7 = 
    [ l
    | l <- lineitems
    , (view -> (ok, nrItems)) <- sumPerOrder
    , l_orderkeyQ l == ok
    , nrItems == maximum(map snd sumPerOrder)
    ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: QA a => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugVL "jan-q7" conn q

main :: IO ()
main = debugQ jan_q7
