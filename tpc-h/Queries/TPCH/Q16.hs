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

-- TPC-H Q16

module Queries.TPCH.Q16
    ( q16
    ) where

import Database.DSH
import Schema.TPCH

fourth :: (QA a, QA b, QA c, QA d) => Q (a, b, c, d) -> Q d
fourth (view -> (_, _, _, d)) = d

q16 :: Q [((Text, Text, Integer), Integer)]
q16 =
  map (\g -> pair (fst g) (length $ nub $ map fourth $ snd g)) $
  groupWithKey (\(view -> (b, t, s, _)) -> tup3 b t s) $
  [ tup4 (p_brandQ p) (p_typeQ p) (p_sizeQ p) (ps_suppkeyQ ps)
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
