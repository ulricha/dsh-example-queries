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
    
-- TPC-H Q3

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

project 
  :: Q ((Integer, Integer, Integer), [((Integer, Integer, Integer), (Double, Double))])
  -> Q ((Integer, Integer, Integer), Double)
project gk = pair (fst gk) revenue
  where
    revenue = sum [ ep * (1 - d) | (view -> (ep, d)) <- [ snd x | x <- snd gk ] ]
    
byRevDate :: Q ((Integer, Integer, Integer), Double) -> Q (Double, Integer)
byRevDate (view -> (((view -> (_, _, sp)), r))) = pair (r * (-1)) sp

q3 :: Q [((Integer, Integer, Integer), Double)]
q3 =
  sortWith byRevDate $
  map project $
  groupWithKey fst $
  [ let sep = tuple3 (l_orderkeyQ l) (o_orderdateQ o) (o_shippriorityQ o)
    in pair sep (pair (l_extendedpriceQ l) (l_discountQ l))
  | c <- customers
  , o <- orders
  , l <- lineitems
  , c_mktsegmentQ c == (toQ "foo")
  , c_custkeyQ c == o_custkeyQ o
  , l_orderkeyQ l == o_orderkeyQ o
  , o_orderdateQ o < (toQ 42)
  , l_shipdateQ l > (toQ 23)
  ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugTAOpt "q3" conn q

main :: IO ()
main = debugQ q3
