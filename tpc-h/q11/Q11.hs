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
    
-- TPC-H Q11

module Main where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Records

fst3 :: (QA a, QA b, QA c) => Q (a, b ,c) -> Q a
fst3 (view -> (a, _, _)) = a

nationPartsValues :: Text -> Q [(Integer, Double, Integer)]
nationPartsValues nation =
  [ tuple3 (ps_partkeyQ ps)
  	   (ps_supplycostQ ps)
	   (ps_availqtyQ ps)
  | ps <- partsupps
  , s  <- suppliers
  , n  <- nations
  , ps_suppkeyQ ps == s_suppkeyQ s
  , s_nationkeyQ s == n_nationkeyQ n
  , n_nameQ n == toQ nation
  ]

totalValue :: Double -> Q Double
totalValue fraction = 
  toQ fraction * sum [ ps_supplycostQ ps * integerToDouble (ps_availqtyQ ps)
                     | ps <- partsupps
	             , s  <- suppliers
	             , n  <- nations
	             , ps_suppkeyQ ps == s_suppkeyQ s
	             , s_nationkeyQ s == n_nationkeyQ n
	             , n_nameQ n == "GERMANY"
  	             ]

partValue :: Q [(Integer, Double, Integer)] -> Q Double
partValue g = sum [ supplycost * integerToDouble availqty 
                  | (view -> (_, supplycost, availqty)) <- g 
		  ]

q11 :: Text -> Double -> Q [(Integer, Double)]
q11 nation fraction = 
  [ pair k (partValue g)
  | (view -> (k, g)) <- groupWithKey fst3 (nationPartsValues nation)
  , partValue g > (totalValue fraction)
  ]

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"

debugQ :: (Show a, QA a) => String -> Q a -> IO ()
debugQ s q = getConn P.>>= \conn -> debugVLOpt s conn q

main :: IO ()
main = debugQ "q11" (q11 "GERMANY" 0.0001)
