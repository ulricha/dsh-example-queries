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

-- | TPC-H Q21
module Queries.TPCH.Q22
    ( q22
    , q22Default
    ) where

import           Database.DSH
import           Queries.TPCH.BuildingBlocks
import           Schema.TPCH

-- Average customer account balance
avgBalance :: Q [Customer] -> Q Decimal
avgBalance cs = avg [ c_acctbalQ c | c <- cs , c_acctbalQ c > 0]

potentialCustomers :: Q [Customer] -> Q [Customer]
potentialCustomers cs =
    [ c
    | c <- cs
    , c_acctbalQ c > avgBalance cs
    , null $ custOrders c
    ]

countryCodeOf :: Q Customer -> Q Text
countryCodeOf c = subString 1 2 (c_phoneQ c)

livesIn :: Q Customer -> [Text] -> Q Bool
livesIn c countries = countryCodeOf c `elem` toQ countries

-- TPC-H query Q22
q22 :: [Text] -> Q [(Text, Integer, Decimal)]
q22 countries =
    sortWith (\(view -> (country, _, _)) -> country)
    [ tup3 country (length custs) (sum (map c_acctbalQ custs))
    | (view -> (country, custs)) <- groupWithKey countryCodeOf pots
    ]
  where
    pots = potentialCustomers [ c | c <- customers, c `livesIn` countries ]

-- | TPC-H Q22 with standard validation parameters
q22Default :: Q [(Text, Integer, Decimal)]
q22Default = q22 ["13", "31", "23", "29", "30", "18", "17"]
