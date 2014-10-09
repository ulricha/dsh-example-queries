module Queries.TPCH.Common where

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

data Interval = Interval { iv_start :: Integer, iv_end :: Integer }

inInterval :: Q Integer -> Interval -> Q Bool
inInterval d interval = d >= toQ (iv_start interval) && d < toQ (iv_end interval)
