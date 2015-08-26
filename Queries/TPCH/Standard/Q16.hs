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

-- | TPC-H Q16
module Queries.TPCH.Standard.Q16
    ( q16
    ) where

import Database.DSH
import Schema.TPCH

byDescCount :: Q ((Text, Text, Integer), Integer) -> Q (Integer, Text, Text, Integer)
byDescCount (view -> (g, c)) = case view g of
    (b, t, s) -> tup4 (-1 * c) b t s

-- | TPC-H Query Q16
q16 :: Q [((Text, Text, Integer), Integer)]
q16 = sortWith byDescCount
      $ groupAggr fst snd (length . nub)
      [ tup2 (tup3 (p_brandQ p) (p_typeQ p) (p_sizeQ p)) (ps_suppkeyQ ps)
      | ps <- partsupps
      , p  <- parts
      , p_partkeyQ p == ps_partkeyQ ps
      , p_brandQ p /= "Brand#45"
      , not (p_typeQ p `like` "MEDIUM POLISHED%")
      , p_sizeQ p `elem` (toQ [49, 14, 23, 45, 19, 3, 36, 9])
      , not (ps_suppkeyQ ps `elem` [ s_suppkeyQ s
                                   | s <- suppliers
                                   , s_commentQ s `like` "%Customer%Complaints%"
                                   ])
      ]
