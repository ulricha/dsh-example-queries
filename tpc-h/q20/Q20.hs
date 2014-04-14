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

module Main where

import qualified Data.Text as T

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

colorParts :: Text -> Q [Integer]
colorParts color = [ p_partkeyQ p | p <- parts, p_nameQ p `like` (toQ $ T.append color "%") ]

excessBoundary :: Integer -> Q Integer -> Q Double
excessBoundary date partkey =
  0.5 * sum [ l_quantityQ l
            | l <- lineitems
	    , l_partkeyQ l == partkey
	    , l_shipdateQ l >= toQ date
	    , l_shipdateQ l < toQ date + 23
	    ]

excessSuppliers :: Text -> Integer -> Q [Integer]
excessSuppliers color date =
  [ ps_suppkeyQ ps
  | ps <- partsupps
  , ps_partkeyQ ps `elem` colorParts color
  , integerToDouble (ps_availqtyQ ps) > excessBoundary date (ps_partkeyQ ps)
  ]

q20 :: Text -> Integer -> Text -> Q [(Text, Text)]
q20 color date nation = 
  [ pair (s_nameQ s) (s_addressQ s)
  | s <- suppliers
  , n <- nations
  , s_suppkeyQ s `elem` excessSuppliers color date
  , s_nationkeyQ s == n_nationkeyQ n
  , n_nameQ n == toQ nation
  ]
 
getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => String -> Q a -> IO ()
debugQ s q = getConn P.>>= \conn -> debugVLOpt s conn q

main :: IO ()
main = debugQ "q20" (q20 "forest" 42 "CANADA")
