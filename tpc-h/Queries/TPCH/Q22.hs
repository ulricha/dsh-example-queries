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
    
-- TPC-H Q21

module Queries.TPCH.Q22
    ( q22
    ) where

import qualified Data.Text as T

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import Database.HDBC.PostgreSQL

import Queries.TPCH.Records

customersAvgBalance :: [Text] -> Q Double
customersAvgBalance areaPrefixes =
  avg [ c_acctbalQ c | c <- customers, c_acctbalQ c > 0, c_phoneQ c `elem` toQ areaPrefixes ]

potentialCustomers :: [Text] -> Q [(Text, Double)]
potentialCustomers areaPrefixes =
  [ pair (c_phoneQ c) (c_acctbalQ c)
  | c <- customers
  , c_phoneQ c `elem` toQ areaPrefixes
  , c_acctbalQ c > customersAvgBalance areaPrefixes
  , null [ 1 :: Q Integer | o <- orders, o_custkeyQ o == c_custkeyQ c ]
  ]

q22 :: [Text] -> Q [(Text, Integer, Double)]
q22 areaPrefixes = 
  sortWith (\(view -> (c, _, _)) -> c) $
  [ tuple3 cntrycode
           (length pas)
	   (sum $ map snd pas)
  | (view -> (cntrycode, pas)) <- groupWithKey fst (potentialCustomers areaPrefixes)
  ]
