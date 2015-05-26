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

module Queries.TPCH.Q11
    ( q11
    , q11Default
    ) where

import Database.DSH
import Schema.TPCH

fst3 :: (QA a, QA b, QA c) => Q (a, b ,c) -> Q a
fst3 (view -> (a, _, _)) = a

nationPartsValues :: Text -> Q [(Integer, Decimal, Integer)]
nationPartsValues nationName =
  [ tup3 (ps_partkeyQ ps)
         (ps_supplycostQ ps)
         (ps_availqtyQ ps)
  | ps <- partsupps
  , s  <- suppliers
  , n  <- nations
  , ps_suppkeyQ ps == s_suppkeyQ s
  , s_nationkeyQ s == n_nationkeyQ n
  , n_nameQ n == toQ nationName
  ]

totalValue :: Decimal -> Q Decimal
totalValue fraction =
  toQ fraction * sum [ ps_supplycostQ ps * integerToDecimal (ps_availqtyQ ps)
                     | ps <- partsupps
                     , s  <- suppliers
                     , n  <- nations
                     , ps_suppkeyQ ps == s_suppkeyQ s
                     , s_nationkeyQ s == n_nationkeyQ n
                     , n_nameQ n == "GERMANY"
                     ]

partValue :: Q [(Integer, Decimal, Integer)] -> Q Decimal
partValue g = sum [ supplycost * integerToDecimal availqty
                  | (view -> (_, supplycost, availqty)) <- g
                  ]

-- | TPC-H Query Q11 with standard validation parameters
q11Default :: Q [(Integer, Decimal)]
q11Default = q11 "GERMANY" 0.0001

-- | TPC-H Query Q11
q11 :: Text -> Decimal -> Q [(Integer, Decimal)]
q11 nationName fraction =
  [ pair k (partValue g)
  | (view -> (k, g)) <- groupWithKey fst3 (nationPartsValues nationName)
  , partValue g > (totalValue fraction)
  ]
