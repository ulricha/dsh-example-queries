{-# LANGUAGE FlexibleContexts      #-}
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

-- | Queries over the stock trading schema from the AQuery paper by Lerner and
-- Shasha (VLDB 2003).
module Queries.AQuery.Trades
    ( bestProfit
    , last10
    ) where

import           Database.DSH
import           Prelude       ()

import           Schema.AQuery

--------------------------------------------------------------------------------
-- For a given date and stock, compute the best profit obtained by
-- buying the stock and selling it later.
--
-- The SQL query shown in the original paper:
--
-- select max(running_diff)
-- from (select t_tid, t_tradedate,
--              t_price - min(t_price) over
--                (partition by t_tid, t_tradedate
-- 	        order by t_timestamp
--                 rows unbounded preceding)
--                as running_diff
--       from trades) as t1
-- where t_tid = 94 and t_tradedate = 15;S
--
-- The same query with the predicate pushed down:
--
-- select max(running_diff)
-- from (select t_price - min(t_price) over
--                (order by t_timestamp
--                 rows unbounded preceding)
--                as running_diff
--       from trades
--       where t_tid = 94 and t_tradedate = 15) as t1;

-- | Running minimum: For each list element, compute the minimum of its
-- predecessors.
mins :: (QA a, TA a, Ord a) => Q [a] -> Q [a]
mins xs = [ minimum [ y | (view -> (y, j)) <- number xs, j <= i ]
          | (view -> (_, i)) <- number xs
          ]

margins :: (Ord a, Num (Q a), QA a, TA a) => Q [a] -> Q [a]
margins xs = [ x - y | (view -> (x,y)) <- zip xs (mins xs) ]

-- | Our profit is the maximum margin obtainable
profit :: (Ord a, Num a, Num (Q a), QA a, TA a) => Q [a] -> Q a
profit xs = maximum (margins xs)

-- | Pest profit obtainable for stock on given date
bestProfit :: Integer -> Day -> Q Double
bestProfit stock date =
    profit [ t_priceQ t
           | t <- sortWith t_timestampQ trades
           , t_tidQ t == toQ stock
           , t_tradeDateQ t == toQ date
           ]

--------------------------------------------------------------------------------
-- Compute the ten last stocks for each quote in a portfolio.

lastn :: QA a => Integer -> Q [a] -> Q [a]
lastn n xs = drop (length xs - toQ n) xs

last10 :: Integer -> Q [(Integer, [Double])]
last10 portfolioId =
    map (\(view -> (tid, g)) -> pair tid (map snd $ lastn 10 g))
    $ groupWithKey fst
    [ pair (t_tidQ t) (t_priceQ t)
    | t <- trades
    , p <- portfolios
    , t_tidQ t == po_tidQ p
    , po_pidQ p == toQ portfolioId
    ]
