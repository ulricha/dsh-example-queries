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

customersAvgBalance :: Q [Customer] -> Q Decimal
customersAvgBalance cs =
    avg [ c_acctbalQ c | c <- cs , c_acctbalQ c > 0]

potentialCustomers :: [Text] -> Q [(Text, Decimal)]
potentialCustomers areaPrefixes =
    [ pair (subString 1 2 (c_phoneQ c)) (c_acctbalQ c)
    | c <- customersFrom areaPrefixes
    , c_acctbalQ c > customersAvgBalance (customersFrom areaPrefixes)
    , c_custkeyQ c `notElem` (map o_custkeyQ orders)
    ]

-- | TPC-H Q22
q22 :: [Text] -> Q [(Text, (Integer, Decimal))]
q22 areaPrefixes =
    sortWith fst
    [ pair cntrycode (pair (length pas) (sum $ map snd pas))
    | (view -> (cntrycode, pas)) <- groupWithKey fst (potentialCustomers areaPrefixes)
    ]

-- | TPC-H Q22 with standard validation parameters
q22Default :: Q [(Text, (Integer, Decimal))]
q22Default = q22 ["13", "31", "23", "29", "30", "18", "17"]
