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

-- TPC-H Q4

module Queries.TPCH.Q4
    ( q4
    , q4'
    , q4''
    ) where

import qualified Data.Time.Calendar  as C
import           Database.DSH
import           Queries.TPCH.Common
import           Schema.TPCH

q4 :: Day -> Q [(Text, Integer)]
q4 startDate =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= toQ startDate
    , o_orderdateQ o < toQ (C.addDays 90 startDate)
    , not $ null [ toQ ()
                 | l <- lineitems
                 , l_orderkeyQ l == o_orderkeyQ o
                 , l_commitdateQ l < l_receiptdateQ l
                 ]
    ]

--------------------------------------------------------------------------------

q4' :: Day -> Q [(Text, Integer)]
q4' startDate =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= toQ startDate
    , o_orderdateQ o < toQ (C.addDays 90 startDate)
    , any (\l -> l_commitdateQ l < l_receiptdateQ l
                 && l_orderkeyQ l == o_orderkeyQ o)
          lineitems
    ]

--------------------------------------------------------------------------------

-- | Is at least one of the orders' items overdue?
hasOverdueItem :: Q Order -> Q Bool
hasOverdueItem o = or [ l_commitdateQ l < l_receiptdateQ l
                      | l <- lineitems
                      , l_orderkeyQ l == o_orderkeyQ o
                      ]

-- | An order is problematic if at least one of its items was overdue.
problematicOrders :: Interval -> Q [Text]
problematicOrders interval =
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o `inInterval` interval
    , hasOverdueItem o
    ]

-- | Compute the number of problematic orders per priority level.
q4'' :: Interval -> Q [(Text, Integer)]
q4'' interval =
    sortWith fst [ tup2 op (length g) | (view -> (op, g)) <- groupWithKey id oids]
    -- sortWith fst [ (op, length g) | (op, g) <- groupWith id oids]
  where
    oids = problematicOrders interval

{-
    [ tup2 op (length $ filter (== op) oids)
    | op <- nub oids
    ]
-}

--------------------------------------------------------------------------------

{-

hasOverdueItem :: Q Order -> Q Bool
hasOverdueItem o =
    any (\l -> l_commitdateQ l < l_receiptdateQ l
               && l_orderkeyQ l == o_orderkeyQ o)
        lineitems

q4'' =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= 42
    , o_orderdateQ o < 42 + 57
    , hasOverdueItem o
    ]

--------------------------------------------------------------------------------

hasOverdueItem' :: Q Order -> Q Bool
hasOverdueItem' o = or [ l_commitdateQ l < l_receiptdateQ l
                       | l <- lineitems
                       , l_orderkeyQ l == o_orderkeyQ o
                       ]

q4''' :: Q [(Text, Integer)]
q4''' =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ groupWithKey id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= 42
    , o_orderdateQ o < 42 + 57
    , hasOverdueItem' o
    ]

{-
q4''' :: Q [(Text, Integer)]
q4''' =
  sortWith fst
  $ map (\(k, g) -> (k, length g))
  $ groupWith id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= 42
    , o_orderdateQ o < 42 + 57
    , hasOverdueItem' o
    ]
-}

--------------------------------------------------------------------------------

group' :: (QA a, QA b, TA b, Eq b) => (Q a -> Q b) -> Q [a] -> Q [(b, [a])]
group' p as =
    [ tup2 k [ a' | a' <- as, p a' == k ]
    | k <- nub $ map p as
    ]

q4''' :: Q [(Text, Integer)]
q4''' =
  sortWith fst
  $ map (\(view -> (k, g)) -> pair k (length g))
  $ group' id
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= 42
    , o_orderdateQ o < 42 + 57
    , hasOverdueItem' o
    ]

--------------------------------------------------------------------------------

data Interval = Interval { iv_start :: Integer, iv_end :: Integer }

problematicOrders :: Interval -> Q [Order]
problematicOrders interval o =
    [ o_orderpriorityQ o
    | o <- orders
    , o_orderdateQ o >= iv_start interval
    , o_orderdateQ o < iv_end interval
    , hasOverdueItem' o
    ]

q4'''' :: Interval -> Q [(Text, Integer)]
q4'''' interval =
    [ tup2 op (length $ filter (\op' -> op == op') problematicOrders)
    | op <- nub problematicOrders
    ]

--------------------------------------------------------------------------------

-}

