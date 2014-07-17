module Queries.TPCH 
    ( module Queries.TPCH.Q1
    , module Queries.TPCH.Q2
    , module Queries.TPCH.Q3
    , module Queries.TPCH.Q4
    , module Queries.TPCH.Q5
    , module Queries.TPCH.Q6
    , module Queries.TPCH.Q7
    , module Queries.TPCH.Q8
    , module Queries.TPCH.Q9
    , module Queries.TPCH.Q10
    , module Queries.TPCH.Q11
    , module Queries.TPCH.Q12
    , module Queries.TPCH.Q14
    , module Queries.TPCH.Q15
    , module Queries.TPCH.Q16
    , module Queries.TPCH.Q17
    , module Queries.TPCH.Q18
    , module Queries.TPCH.Q19
    , module Queries.TPCH.Q20
    , module Queries.TPCH.Q21
    , module Queries.TPCH.Q22
    , getConn
    ) where

import Database.HDBC.PostgreSQL

import Queries.TPCH.Q1
import Queries.TPCH.Q2
import Queries.TPCH.Q3
import Queries.TPCH.Q4
import Queries.TPCH.Q5
import Queries.TPCH.Q6
import Queries.TPCH.Q7
import Queries.TPCH.Q8
import Queries.TPCH.Q9
import Queries.TPCH.Q10
import Queries.TPCH.Q11
import Queries.TPCH.Q12
import Queries.TPCH.Q14
import Queries.TPCH.Q15
import Queries.TPCH.Q16
import Queries.TPCH.Q17
import Queries.TPCH.Q18
import Queries.TPCH.Q19
import Queries.TPCH.Q20
import Queries.TPCH.Q21
import Queries.TPCH.Q22

getConn :: IO Connection
getConn = connectPostgreSQL "user = 'au' password = 'foobar' host = 'localhost' port = '5432' dbname = 'tpch'"
