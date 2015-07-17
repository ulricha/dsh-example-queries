{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | Benchmark nested query execution using multiple execution strategies:
--   * Running nested queries with DSH
--   * Return nested results in JSON form from a single SQL query
--   * Nested loop query execution (n+1)
module Main where

-- Nested.custFromOrders
-- Nested.custRevenues
-- Nested.expectedRevenueFor
-- Nested.shippingDelay
-- Nested.shippingDelayInterval
-- TopK.topOrdersPerCust'
-- TopK.regionsTopCustomers

import           Control.Monad

import           Control.DeepSeq
import qualified Criterion                  as B
import qualified Criterion.Main             as M
import qualified Data.Aeson                 as A
import qualified Data.Aeson.Types           as AT
import qualified Data.Attoparsec.Text       as AP
import qualified Data.ByteString.Char8      as BS
import qualified Data.Convertible           as Conv
import qualified Data.Decimal               as D
import qualified Data.Scientific            as S
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Time.Calendar         as D
import qualified Data.Vector                as V
import qualified Database.HDBC              as H
import qualified Database.HDBC.ODBC         as O
import qualified Database.PostgreSQL.Simple as PG

import qualified Database.DSH               as Q
import qualified Database.DSH.Backend.Sql   as DS
import qualified Database.DSH.Compiler      as C

import           Queries.TPCH.NonStandard

newtype Date = Date { unDate :: D.Day } deriving (Show)

instance NFData Date where
    rnf (Date d) = rnf $! d

instance A.FromJSON Date where
    parseJSON = A.withText "datestring" dateParser

dateParser :: T.Text -> AT.Parser Date
dateParser t =
    case AP.parseOnly (attoDate <* AP.endOfInput) t of
        Left msg -> fail msg
        Right d  -> return d
  where
    attoDate :: AP.Parser Date
    attoDate = do
        y <- AP.decimal
        void $ AP.char '-'
        m <- AP.decimal
        void $ AP.char '-'
        d <- AP.decimal
        maybe mempty (return . Date) (D.fromGregorianValid y m d)

--------------------------------------------------------------------------------

pgConn :: IO PG.Connection
pgConn = PG.connectPostgreSQL "host='localhost' port='5432' dbname='tpch1test' user='au' password='foobar'"

hdbcConn :: IO O.Connection
hdbcConn = O.connectODBC "DSN=tpch1test"

--------------------------------------------------------------------------------
-- Query custFromOrders

custFromOrdersSqlLateral :: PG.Query
custFromOrdersSqlLateral =
    "select c.c_name, json_agg(json_build_array(i.o_orderpriority, i.o_totalprice))\n\
    \from customer c,\n\
    \     lateral (select o.o_orderpriority, o.o_totalprice\n\
    \              from orders o\n\
    \              where c.c_custkey = o.o_custkey) i\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n.n_name = 'GERMANY')\n\
    \  and c.c_custkey in (select o.o_custkey from orders o)\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

custFromOrdersSqlLateralP :: PG.Connection -> IO [(T.Text, [(T.Text, Double)])]
custFromOrdersSqlLateralP c = do
    vRes <- PG.query_ c custFromOrdersSqlLateral

    let convertRow :: (T.Text, A.Value) -> (T.Text, [(T.Text, Double)])
        convertRow (t, v) = (t, jsonTuples v)

        jsonTuples :: A.Value -> [(T.Text, Double)]
        jsonTuples v = case A.fromJSON v of
                          A.Error msg -> error msg
                          A.Success a -> a

    return $ map convertRow vRes

custFromOrdersSqlAvalanche :: PG.Connection -> IO [(T.Text, [(T.Text, S.Scientific)])]
custFromOrdersSqlAvalanche c = do
    custs <- PG.query_ c "select c.c_name, c.c_custkey\n\
                         \from customer c\n\
                         \where c.c_nationkey in (select n.n_nationkey\n\
                         \                        from nation n\n\
                         \                        where n.n_name = 'GERMANY')\n\
                         \  and c.c_custkey in (select o.o_custkey from orders o)"

    forM custs $ \(custName, custKey) -> do
        os <- PG.query c "select o.o_orderpriority, o.o_totalprice\n\
                         \from orders o\n\
                         \where o.o_custkey = ?" (PG.Only (custKey::Int))
        return (custName, os)

custFromOrdersInHeap :: PG.Connection -> IO [(T.Text, [(T.Text, S.Scientific)])]
custFromOrdersInHeap c = do
    custs   <- PG.query_ c "select c.c_name, c.c_custkey, c.c_nationkey from customer c" :: IO [(T.Text, Int, Int)]
    orders  <- PG.query_ c "select o.o_orderpriority, o.o_totalprice, o.o_custkey from orders o" :: IO [(T.Text, S.Scientific, Int)]
    nations <- PG.query_ c "select n.n_name, n.n_nationkey from nation n" :: IO [(T.Text, Int)]

    return [ ( cName
             , [ (oPrio, oTotalPrice)
               | (oPrio, oTotalPrice, oCustKey) <- orders
               , oCustKey == cKey
               ])
           | (cName, cKey, cNationKey) <- custs
           , cNationKey `elem` [ nKey | (nName, nKey) <- nations, nName == "GERMANY" ]
           , cKey `elem` [ oCustKey | (_, _, oCustKey) <- orders ]
           ]

custFromOrdersDSH :: O.Connection -> IO [(T.Text, [(T.Text, Q.Decimal)])]
custFromOrdersDSH c = C.runQ (DS.sqlBackend c) $ custFromOrders "GERMANY"

--------------------------------------------------------------------------------

custRevenuesSqlLateralDeep :: PG.Query
custRevenuesSqlLateralDeep =
    "select c.c_name, array_agg(i.r)\n\
    \from customer c,\n\
    \     lateral (select sum(ls.l_extendedprice * (1 - ls.l_discount)) as r\n\
    \              from orders o,\n\
    \                   lateral (select l.l_extendedprice, l.l_discount\n\
    \                            from lineitem l\n\
    \                            where o.o_orderkey = l.l_orderkey) ls\n\
    \              where o.o_custkey = c.c_custkey\n\
    \              group by o.o_orderkey) i\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n.n_name = 'GERMANY')\n\
    \  and c.c_custkey in (select o.o_custkey from orders o)\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

custRevenuesSqlLateral :: PG.Query
custRevenuesSqlLateral =
    "select c.c_name, array_agg(i.r)\n\
    \from customer c,\n\
    \     lateral (select sum(l.l_extendedprice * (1 - l.l_discount)) as r\n\
    \              from orders o,\n\
    \                   lineitem l\n\
    \              where o.o_custkey = c.c_custkey\n\
    \                and o.o_orderkey = l.l_orderkey\n\
    \              group by o.o_orderkey) i\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n.n_name = 'GERMANY')\n\
    \  and c.c_custkey in (select o.o_custkey from orders o)\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

custRevenuesSqlAvalanche :: PG.Connection -> IO [(T.Text, [S.Scientific])]
custRevenuesSqlAvalanche c = do
    custs <- PG.query_ c "select c.c_name, c.c_custkey\n\
                         \from customer c\n\
                         \where c.c_nationkey in (select n.n_nationkey\n\
                         \                        from nation n\n\
                         \                        where n.n_name = 'GERMANY')\n\
                         \  and c.c_custkey in (select o.o_custkey from orders o)"

    forM custs $ \(custName, custKey) -> do
        rs <- PG.query c "select sum(l.l_extendedprice * (1 - l.l_discount)) as r\n\
                         \from orders o, lineitem l\n\
                         \where o.o_orderkey = l.l_orderkey\n\
                         \  and o.o_custkey = ?\n\
                         \group by o.o_orderkey" (PG.Only (custKey :: Int))
        return (custName, map PG.fromOnly rs)

custRevenuesSqlLateralP :: PG.Query -> PG.Connection -> IO [(T.Text, [S.Scientific])]
custRevenuesSqlLateralP q c = do
    vRes <- PG.query_ c q :: IO [(T.Text, V.Vector S.Scientific)]
    return $ map (\(t, v) -> (t, V.toList v)) vRes

custRevenuesDSH :: O.Connection -> IO [(T.Text, [Q.Decimal])]
custRevenuesDSH c = C.runQ (DS.sqlBackend c) $ custRevenues "GERMANY"

--------------------------------------------------------------------------------

expectedRevenueForSql :: PG.Query
expectedRevenueForSql =
    "select c.c_name, json_agg(json_build_array(os.o_orderdate, os.r))\n\
    \from customer c,\n\
    \     (select o.o_custkey, o.o_orderdate,\n\
    \             sum(l.l_extendedprice * (1 - l.l_discount)) as r\n\
    \      from orders o, lineitem l\n\
    \      where o.o_orderkey = l.l_orderkey\n\
    \      and   o.o_orderstatus = 'P'\n\
    \      group by o.o_orderkey, o.o_custkey, o.o_orderdate) os\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n_name = 'GERMANY')\n\
    \and c.c_custkey in (select o.o_custkey\n\
    \                    from orders o\n\
    \                    where o.o_orderstatus = 'P')\n\
    \and c.c_custkey = os.o_custkey\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

expectedRevenueForSqlLateral :: PG.Query
expectedRevenueForSqlLateral =
    "select c.c_name, json_agg(json_build_array(os.o_orderdate, os.r))\n\
    \from customer c,\n\
    \     lateral (select o.o_orderdate,\n\
    \                     sum(l.l_extendedprice * (1 - l.l_discount)) as r\n\
    \                     from orders o, lineitem l\n\
    \              where o.o_orderkey = l.l_orderkey\n\
    \              and   o.o_orderstatus = 'P'\n\
    \              and   o.o_custkey = c.c_custkey\n\
    \              group by o.o_orderkey, o.o_custkey, o.o_orderdate) os\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n_name = 'GERMANY')\n\
    \and c.c_custkey in (select o.o_custkey\n\
    \                    from orders o\n\
    \                    where o.o_orderstatus = 'P')\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

expectedRevenueForSqlP :: PG.Query -> PG.Connection -> IO [(T.Text, [(Date, Double)])]
expectedRevenueForSqlP q c = do
    jRes <- PG.query_ c q

    let convertRow :: (T.Text, A.Value) -> (T.Text, [(Date, Double)])
        convertRow (t, v) = (t, jsonTuples v)

        jsonTuples :: A.Value -> [(Date, Double)]
        jsonTuples v = case A.fromJSON v of
                          A.Error msg -> error msg
                          A.Success a -> a

    return $ map convertRow jRes

expectedRevenueForSqlAvalanche :: PG.Connection -> IO [(T.Text, [(D.Day, S.Scientific)])]
expectedRevenueForSqlAvalanche c = do
    custs <- PG.query_ c "select c.c_name, c.c_custkey\n\
                         \from customer c\n\
                         \where c.c_nationkey in (select n.n_nationkey\n\
                         \                        from nation n\n\
                         \                        where n.n_name = 'GERMANY')\n\
                         \  and c.c_custkey in (select o.o_custkey\n\
                         \                     from orders o\n\
                         \                     where o.o_orderstatus = 'P')"

    forM custs $ \(custName, custKey) -> do
        rs <- PG.query c "select o.o_orderdate, sum(l.l_extendedprice * (1 - l.l_discount)) as r\n\
                         \from orders o, lineitem l\n\
                         \where o.o_orderkey = l.l_orderkey\n\
                         \  and o.o_orderstatus = 'P'\n\
                         \  and o.o_custkey = ?\n\
                         \group by o.o_orderkey" (PG.Only (custKey :: Int))
        return (custName, rs)

expectedRevenueForDSH :: O.Connection -> IO [(T.Text, [(D.Day, Q.Decimal)])]
expectedRevenueForDSH c = C.runQ (DS.sqlBackend c) $ expectedRevenueFor "GERMANY"

--------------------------------------------------------------------------------

-- Native SQL implementation based on arrays and LATERAL
shippingDelaySqlLateral :: PG.Query
shippingDelaySqlLateral =
    "select o.o_orderkey,\n\
    \       array_sort(array_agg(ls.l_quantity)) as qs,\n\
    \       avg(ls.l_shipdate - o.o_orderdate)\n\
    \from orders o,\n\
    \     lateral (select l.l_shipdate, l.l_quantity\n\
    \              from lineitem l\n\
    \              where l.l_orderkey = o.o_orderkey) ls\n\
    \group by o.o_orderkey\n\
    \order by o.o_orderkey;"

shippingDelaySqlLateralRunP :: PG.Connection -> IO [(Integer, [S.Scientific], S.Scientific)]
shippingDelaySqlLateralRunP c = do
    vRes <- PG.query_ c shippingDelaySqlLateral
    return $ map (\(k, v, a) -> (k, V.toList v, a)) vRes

shippingDelayAvalanche :: PG.Connection -> IO [(Integer, [S.Scientific], S.Scientific)]
shippingDelayAvalanche c = do
    orders <- PG.query_ c "select o.o_orderkey, avg(l.l_shipdate - o.o_orderdate)\n\
                          \from orders o,\n\
                          \     lineitem l\n\
                          \where o.o_orderkey = l.l_orderkey\n\
                          \group by o.o_orderkey\n\
                          \order by o.o_orderkey;"

    forM orders $ \(oKey, avg) -> do
        items <- PG.query c "select l.l_quantity\n\
                            \from lineitem l\n\
                            \where l.l_orderkey = ?\n\
                            \order by l.l_shipdate" (PG.Only (oKey :: Integer))

        return (oKey, map PG.fromOnly items, avg)

shippingDelayDSH :: O.Connection -> IO [(Integer, [Q.Decimal], Double)]
shippingDelayDSH c = C.runQ (DS.sqlBackend c) shippingDelay

--------------------------------------------------------------------------------

-- Native SQL implementation based on arrays and LATERAL
shippingDelayIntervalSqlLateral :: PG.Query
shippingDelayIntervalSqlLateral =
    "select o.o_orderkey,\n\
    \       array_sort(array_agg(ls.l_quantity)) as qs,\n\
    \       avg(ls.l_shipdate - o.o_orderdate)\n\
    \from orders o,\n\
    \     lateral (select l.l_shipdate, l.l_quantity\n\
    \              from lineitem l\n\
    \              where l.l_orderkey = o.o_orderkey) ls\n\
    \where ((o.o_orderdate < DATE '1993-10-01')\n\
    \  and (o.o_orderdate >= DATE '1993-07-01'))\n\
    \group by o.o_orderkey\n\
    \order by o.o_orderkey;"

shippingDelayIntervalSqlLateralRunP :: PG.Connection -> IO [(Integer, [S.Scientific], S.Scientific)]
shippingDelayIntervalSqlLateralRunP c = do
    vRes <- PG.query_ c shippingDelayIntervalSqlLateral
    return $ map (\(k, v, a) -> (k, V.toList v, a)) vRes

shippingDelayIntervalAvalanche :: PG.Connection -> IO [(Integer, [S.Scientific], S.Scientific)]
shippingDelayIntervalAvalanche c = do
    orders <- PG.query_ c "select o.o_orderkey, avg(l.l_shipdate - o.o_orderdate)\n\
                          \from orders o,\n\
                          \     lineitem l\n\
                          \where ((o.o_orderdate < DATE '1993-10-01')\n\
                          \  and (o.o_orderdate >= DATE '1993-07-01'))\n\
                          \  and o.o_orderkey = l.l_orderkey\n\
                          \group by o.o_orderkey\n\
                          \order by o.o_orderkey;"

    forM orders $ \(oKey, avg) -> do
        items <- PG.query c "select l.l_quantity\n\
                            \from lineitem l\n\
                            \where l.l_orderkey = ?\n\
                            \order by l.l_shipdate" (PG.Only (oKey :: Integer))

        return (oKey, map PG.fromOnly items, avg)

shippingDelayIntervalDSH :: O.Connection -> IO [(Integer, [Q.Decimal], Double)]
shippingDelayIntervalDSH c = C.runQ (DS.sqlBackend c) shippingDelayInterval

--------------------------------------------------------------------------------

topOrdersPerCustSql :: PG.Query
topOrdersPerCustSql =
    "select c.c_name, array_agg(ii.o_orderdate)\n\
    \from customer c,\n\
    \     (select i.o_custkey, i.o_orderdate\n\
    \      from (select ics.o_custkey, ics.o_orderdate, row_number() over (partition by ics.o_custkey order by ics.ic) as x\n\
    \            from (select o.o_custkey, o.o_orderdate, count(*) as ic\n\
    \                  from orders o,\n\
    \                       lineitem l\n\
    \                  where o.o_orderkey = l.l_orderkey\n\
    \                  group by o.o_orderkey, o.o_custkey, o.o_orderdate) ics) i\n\
    \      where i.x <= 10) ii\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n.n_name = 'GERMANY')\n\
    \  and c.c_custkey = ii.o_custkey\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

topOrdersPerCustSqlLateral :: PG.Query
topOrdersPerCustSqlLateral =
    "select c.c_name, array_agg(ii.o_orderdate)\n\
    \from customer c,\n\
    \     lateral (select i.o_orderdate\n\
    \              from (select ics.o_orderdate, row_number() over (partition by ics.c_custkey order by ics.ic) as x\n\
    \                    from (select o.o_orderdate, c.c_custkey, count(*) as ic\n\
    \                          from orders o,\n\
    \                               lineitem l\n\
    \                          where o.o_orderkey = l.l_orderkey\n\
    \                            and o.o_custkey = c.c_custkey\n\
    \                          group by o.o_orderkey, o.o_orderdate) ics) i\n\
    \              where i.x <= 10) ii\n\
    \where c.c_nationkey in (select n.n_nationkey\n\
    \                        from nation n\n\
    \                        where n.n_name = 'GERMANY')\n\
    \group by c.c_custkey, c.c_name\n\
    \order by c.c_custkey;"

topOrdersPerCustP :: PG.Query -> PG.Connection -> IO [(T.Text, [D.Day])]
topOrdersPerCustP q c = do
    vRes <- PG.query_ c q
    return $ map (\(t, v) -> (t, V.toList v)) vRes

topOrdersPerCustSqlAvalanche :: PG.Connection -> IO [(T.Text, [D.Day])]
topOrdersPerCustSqlAvalanche c = do
    custs <- PG.query_ c "select c.c_name, c.c_custkey\n\
                         \from customer c\n\
                         \where c.c_nationkey in (select n.n_nationkey\n\
                         \                        from nation n\n\
                         \                        where n.n_name = 'GERMANY')\n\
                         \  and c.c_custkey in (select o.o_custkey from orders o);"

    forM custs $ \(cName, cKey) -> do
        orders <- PG.query c "select o.o_orderdate\n\
                             \from orders o,\n\
                             \     lineitem l\n\
                             \where o.o_custkey = ?\n\
                             \  and o.o_orderkey = l.l_orderkey\n\
                             \group by o.o_orderkey\n\
                             \order by count(*)\n\
                             \limit 10;" (PG.Only (cKey :: Int))

        return (cName, map PG.fromOnly orders)

topOrdersPerCustDSH :: O.Connection -> IO [(T.Text, [D.Day])]
topOrdersPerCustDSH c = C.runQ (DS.sqlBackend c) $ topOrdersPerCust' 10 "GERMANY"

--------------------------------------------------------------------------------

regionsTopCustomersSqlLateral :: PG.Query
regionsTopCustomersSqlLateral =
    "select n.n_name, array_agg(ii.c_name)\n\
    \from region r,\n\
    \     nation n,\n\
    \     lateral (select i.c_name\n\
    \              from (select ocs.c_name, row_number() over (partition by ocs.n\n\
    \                                                          order by ocs.oc) as rn\n\
    \                    from (select c.c_name, n.n_nationkey as n, count(*) as oc\n\
    \                          from customer c,\n\
    \                               orders o\n\
    \                          where c.c_nationkey = n.n_nationkey\n\
    \                            and c.c_custkey = o.o_custkey\n\
    \                          group by c.c_custkey, c.c_name) ocs) i\n\
    \              where i.rn <= 10) ii\n\
    \where r.r_name = 'EUROPE'\n\
    \  and r.r_regionkey = n.n_regionkey\n\
    \group by n.n_nationkey\n\
    \order by n.n_nationkey;"

regionsTopCustomersSqlLateralUnion :: PG.Query
regionsTopCustomersSqlLateralUnion =
    "select n.n_name, array_agg(ii.c_name)\n\
    \from region r,\n\
    \     nation n,\n\
    \     lateral (select i.c_name\n\
    \              from (select ocs.c_name, row_number() over (partition by ocs.n\n\
    \                                                          order by ocs.oc) as rn\n\
    \                    from ((select c.c_name, n.n_nationkey as n, count(*) as oc\n\
    \                          from customer c,\n\
    \                               orders o\n\
    \                          where c.c_nationkey = n.n_nationkey\n\
    \                            and c.c_custkey = o.o_custkey\n\
    \                          group by c.c_custkey, c.c_name)\n\
    \                          union all\n\
    \                          (select c.c_name, n.n_nationkey as n, 0 as oc\n\
    \                           from customer c\n\
    \                           where c.c_nationkey = n.n_nationkey\n\
    \                             and c.c_custkey not in (select o.o_custkey from orders o))) ocs) i\n\
    \              where i.rn <= 10) ii\n\
    \where r.r_name = 'EUROPE'\n\
    \  and r.r_regionkey = n.n_regionkey\n\
    \group by n.n_nationkey\n\
    \order by n.n_nationkey;"

-- SQL version: include customers without orders, using LEFT OUTER JOIN
regionsTopCustomersSqlLateralLeftOuter :: PG.Query
regionsTopCustomersSqlLateralLeftOuter =
    "select n.n_name, array_agg(ii.c_name)\n\
    \from region r,\n\
    \     nation n,\n\
    \     lateral (select i.c_name\n\
    \              from (select ocs.c_name, row_number() over (partition by ocs.n\n\
    \                                                          order by ocs.oc) as rn\n\
    \                    from ((select c.c_name, n.n_nationkey as n, count(o.o_orderkey) as oc\n\
    \                          from customer c\n\
    \                               left outer join\n\
    \                               orders o\n\
    \                               on c.c_custkey = o.o_custkey\n\
    \                          where c.c_nationkey = n.n_nationkey\n\
    \                          group by c.c_custkey, c.c_name)) ocs) i\n\
    \              where i.rn <= 10) ii\n\
    \where r.r_name = 'EUROPE'\n\
    \  and r.r_regionkey = n.n_regionkey\n\
    \group by n.n_nationkey\n\
    \order by n.n_nationkey;"

regionsTopCustomersP :: PG.Query -> PG.Connection -> IO [(T.Text, [T.Text])]
regionsTopCustomersP q c = do
    vRes <- PG.query_ c q

    return $ map (\(t, v) -> (t, V.toList v)) vRes

regionsTopCustomersSqlAvalanche :: PG.Connection -> IO [(T.Text, [T.Text])]
regionsTopCustomersSqlAvalanche c = do
    nations <- PG.query_ c "select n.n_name, n.n_nationkey\n\
                           \from region r,\n\
                           \     nation n\n\
                           \where r.r_name = 'EUROPE'\n\
                           \  and r.r_regionkey = n.n_regionkey"

    forM nations $ \(nName, nKey) -> do
        custs <- PG.query c "select c.c_name\n\
                            \from customer c,\n\
                            \     orders o\n\
                            \where o.o_custkey = c.c_custkey\n\
                            \  and c.c_nationkey = ?\n\
                            \group by c.c_custkey\n\
                            \order by count(*) desc\n\
                            \limit 10;" (PG.Only (nKey :: Int))

        return (nName, map PG.fromOnly custs)

regionsTopCustomersDSH :: O.Connection -> IO [(T.Text, [T.Text])]
regionsTopCustomersDSH c = C.runQ (DS.sqlBackend c) $ regionsTopCustomers "EUROPE" 10

--------------------------------------------------------------------------------

-- Nested.custFromOrders
-- Nested.custRevenues
-- Nested.expectedRevenueFor
-- Nested.shippingDelay
-- Nested.shippingDelayInterval
-- TopK.topOrdersPerCust'
-- TopK.regionsTopCustomers

benchQ :: NFData a => String -> c -> (c -> IO a) -> B.Benchmark
benchQ benchName c q = B.bench benchName $ B.nfIO (q c)

benchmarks :: (O.Connection, PG.Connection) -> [B.Benchmark]
benchmarks (c, c') =
    [ B.bgroup "custFromOrders"
        [ benchQ "custFromOrdersSqlLateral" c' custFromOrdersSqlLateralP
        , benchQ "custFromOrdersSqlAvalanche" c' custFromOrdersSqlAvalanche
        , benchQ "custFromOrdersDSH" c custFromOrdersDSH
        ]
    , B.bgroup "custRevenues"
        [ benchQ "custRevenuesSqlLateral" c' $ custRevenuesSqlLateralP custRevenuesSqlLateral
        , benchQ "custRevenuesSqlLateralDeep" c' $ custRevenuesSqlLateralP custRevenuesSqlLateralDeep
        , benchQ "custRevenuesSqlAvalanche" c' custRevenuesSqlAvalanche
        , benchQ "custRevenuesDSH" c custRevenuesDSH
        ]
    , B.bgroup "expectedRevenueFor"
        [ benchQ "expectedRevenueForSql" c' $ expectedRevenueForSqlP  expectedRevenueForSql
        , benchQ "expectedRevenueForSqlLateral" c' $ expectedRevenueForSqlP expectedRevenueForSqlLateral
        , benchQ "expectedRevenueForSqlAvalanche" c' expectedRevenueForSqlAvalanche
        , benchQ "expectedRevenueForDSH" c expectedRevenueForDSH
        ]
    -- , B.bgroup "shippingDelay"
    --     [ benchQ "shippingDelaySqlLateral" c' shippingDelaySqlLateralRunP
    --     , benchQ "shippingDelayAvalanche" c' shippingDelayAvalanche
    --     , benchQ "shippingDelayDSH" c shippingDelayDSH
    --     ]
    , B.bgroup "shippingDelayInterval"
        [ benchQ "shippingDelayIntervalSqlLateral" c' shippingDelayIntervalSqlLateralRunP
        , benchQ "shippingDelayIntervalAvalanche" c' shippingDelayIntervalAvalanche
        , benchQ "shippingDelayIntervalDSH" c shippingDelayIntervalDSH
        ]
    , B.bgroup "topOrdersPerCust"
        [ benchQ "topOrdersPerCustSql" c' $ topOrdersPerCustP topOrdersPerCustSql
        , benchQ "topOrdersPerCustSqlLateral" c' $ topOrdersPerCustP topOrdersPerCustSqlLateral
        , benchQ "topOrdersPerCustSqlAvalanche" c' topOrdersPerCustSqlAvalanche
        , benchQ "topOrdersPerCustDSH" c topOrdersPerCustDSH
        ]
    , B.bgroup "regionsTopCustomers"
        [ benchQ "regionsTopCustomersSqlLateral" c' $ regionsTopCustomersP regionsTopCustomersSqlLateral
        , benchQ "regionsTopCustomersSqlLateralUnion" c' $ regionsTopCustomersP regionsTopCustomersSqlLateralUnion
        , benchQ "regionsTopCustomersSqlLateralLeftOuter" c' $ regionsTopCustomersP regionsTopCustomersSqlLateralLeftOuter
        , benchQ "regionsTopCustomersSqlAvalanche" c' regionsTopCustomersSqlAvalanche
        , benchQ "regionsTopCustomersDSH" c regionsTopCustomersDSH
        ]
    ]


--------------------------------------------------------------------------------

main :: IO ()
main = do
    c <- hdbcConn
    c' <- pgConn
    M.defaultMain $ benchmarks (c, c')

-- main :: IO ()
-- main = do
--     c  <- pgConn
--     c' <- hdbcConn
--     -- r <- custRevenuesSqlLateralP c (custRevenuesSqlLateral)
--     -- r <- expectedRevenueForSqlP c (expectedRevenueForSqlLateral)
--     -- r <- expectedRevenueForSqlAvalanche c
--     -- r <- expectedRevenueForDSH c'
--     r <- topOrdersPerCustDSH c'
--     putStrLn $ show r
