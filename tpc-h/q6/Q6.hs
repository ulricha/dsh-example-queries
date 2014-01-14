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

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

between :: Q Double -> Q Double -> Q Double -> Q Bool
between x l r = x >= l && x <= r

q6 :: Q Double
q6 = 
  sum $
  [ l_extendedpriceQ l * l_discountQ l
  | l <- lineitems
  , l_shipdateQ l >= 23
  , l_shipdateQ l < 42
  , between (l_discountQ l) ((toQ 0.05) - (toQ 0.01)) ((toQ 0.05) + (toQ 0.01))
  , l_quantityQ l < 24
  ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

{-
runQ :: (Show a,QA a) => Q a -> IO ()
runQ q = getConn P.>>= \conn -> fromQX100 conn q P.>>= P.print
-}

debugQ :: (Show a, QA a) => Q a -> IO ()
debugQ q = getConn P.>>= \conn -> debugTAOpt "q6" conn q

main :: IO ()
main = debugQ q6
