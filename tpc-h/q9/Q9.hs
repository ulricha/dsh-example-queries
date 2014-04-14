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
    
-- TPC-H Q9

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

thd3 :: (QA a, QA b, QA c) => Q (a, b, c) -> Q c
thd3 (view -> (_, _, c)) = c

profit :: Q [(Text, Integer, Double)]
profit = 
  [ tuple3 (n_nameQ n) 
           (o_orderdateQ o)
           (l_extendedpriceQ l * (1 - l_discountQ l) - ps_supplycostQ ps * l_quantityQ l)
  | p <- parts
  , s <- suppliers
  , l <- lineitems
  , ps <- partsupps
  , o <- orders
  , n <- nations
  , s_suppkeyQ s == l_suppkeyQ l
  , ps_suppkeyQ ps == l_suppkeyQ l
  , ps_partkeyQ ps == l_partkeyQ l
  , p_partkeyQ p == l_partkeyQ l
  , o_orderkeyQ o == l_orderkeyQ l
  , s_nationkeyQ s == n_nationkeyQ n
  , p_nameQ p `like` (toQ "%green%")
  ]

q9 :: Q [(Text, Integer, Double)]
q9 =
   sortWith (\(view -> (n, y, _)) -> pair n (y * (-1)))
   [ tuple3 (fst k) (snd k) (sum $ map thd3 g)
   | (view -> (k, g)) <- groupWithKey (\(view -> (n, y, _)) -> pair n y) profit
   ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => String -> Q a -> IO ()
debugQ s q = getConn P.>>= \conn -> debugTAOpt s conn q

main :: IO ()
main = debugQ "q9" q9
