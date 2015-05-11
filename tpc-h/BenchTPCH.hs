-- | Benchmark DSH formulations of TPC-H queries and their native SQL
-- counterparts.
module Main where

import           Control.Monad

import qualified Criterion                as B
import qualified Criterion.Main           as M
import qualified Database.HDBC            as H
import qualified Database.HDBC.ODBC       as O

import qualified Database.DSH             as Q
import qualified Database.DSH.Backend.Sql as S
import qualified Database.DSH.Compiler    as C

import           Queries.TPCH

-- | Run a SQL query, fetch and discard all results.
runRawSql :: O.Connection -> String -> IO ()
runRawSql conn sqlQuery = do
    stmt <- H.prepare conn sqlQuery
    void $ H.execute stmt []
    void $ H.fetchAllRows' stmt

-- | Run a number of SQL queries.
runRawQueries :: O.Connection -> [String] -> IO ()
runRawQueries c qs = mapM_ (runRawSql c) qs

-- | Read a SQL query from a file and run it.
runSqlFromFile :: O.Connection -> FilePath -> IO ()
runSqlFromFile conn sqlFile = readFile sqlFile >>= runRawSql conn

-- | Obtain the raw SQL queries for a DSH query.
dshCompileToFlatQueries :: Q.QA a => S.SqlBackend -> Q.Q a -> [String]
dshCompileToFlatQueries c q = map S.unwrapCode $ C.codeQ c q

-- | Benchmark the execution time of the backend code for a DSH
-- query. The measured time does /not/ include DSH query compilation.
benchmarkDSH :: Q.QA a => String -> O.Connection -> Q.Q a -> B.Benchmark
benchmarkDSH benchName c q =
    B.env (return $ dshCompileToFlatQueries (S.sqlBackend c) q)
          (\qs -> B.bench benchName $ B.nfIO (runRawQueries c qs))

-- | Benchmark a SQL query obtained from a file.
benchmarkNative :: String -> O.Connection -> FilePath -> B.Benchmark
benchmarkNative benchName c q =
    B.env (readFile $ "tpch_native/" ++ q)
          (\q -> B.bench benchName $ B.nfIO (runRawSql c q))

benchmarks :: O.Connection -> [B.Benchmark]
benchmarks c =
    [ B.bgroup "Q1"
        [ benchmarkDSH "q1" c q1'
        , benchmarkNative "q1n" c "q1.sql"
        ]
    ]

main :: IO ()
main = do
    c <- O.connectODBC "DSN=tpch1"
    M.defaultMain $ benchmarks c
