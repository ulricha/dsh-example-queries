{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ViewPatterns          #-}

module Records where

import Database.DSH

-- primary key: l_orderkey, l_linenumber
data LineItem = LineItem
    { l_comment       :: Text
    , l_commitdate    :: Integer
    , l_discount      :: Double
    , l_extendedprice :: Double
    , l_linenumber    :: Integer
    , l_linestatus    :: Text
    , l_orderkey      :: Integer
    , l_partkey       :: Integer
    , l_quantity      :: Double
    , l_receiptdate   :: Integer
    , l_returnflag    :: Text
    , l_shipdate      :: Integer
    , l_shipinstruct  :: Text
    , l_shipmode      :: Text
    , l_suppkey       :: Integer
    , l_tax           :: Double
    }

deriveDSH ''LineItem
deriveTA ''LineItem
generateTableSelectors ''LineItem
