-- | Benchmark SQL queries produced by DSH against native SQL versions of all
-- TPC-H benchmark queries.
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
        [ benchmarkDSH "q1" c q1Default
        , benchmarkNative "q1n" c "q1.sql"
        ]
    , B.bgroup "Q2"
        [ benchmarkDSH "q2" c q2Default
        -- , benchmarkDSH "q2a" c q2aDefault
        , benchmarkNative "q2n" c "q2.sql"
        ]
    , B.bgroup "Q3"
        [ benchmarkDSH "q3" c q3Default
        -- , benchmarkDSH "q3a" c q3Default
        -- , benchmarkDSH "q3b" c q3Default
        , benchmarkNative "q3n" c "q3.sql"
        ]
    , B.bgroup "Q4"
        [ benchmarkDSH "q4" c q4Default
        -- , benchmarkDSH "q4a" c q4aDefault
        -- , benchmarkDSH "q4b" c q4bDefault
        , benchmarkNative "q4n" c "q4.sql"
        ]
    , B.bgroup "Q5"
        [ benchmarkDSH "q5" c q5Default
        , benchmarkNative "q5n" c "q5.sql"
        ]
    , B.bgroup "Q6"
        [ benchmarkDSH "q6" c q6Default
        , benchmarkNative "q6n" c "q6.sql"
        ]
    , B.bgroup "Q7"
        [ benchmarkDSH "q7" c q7Default
        -- , benchmarkDSH "q7a" c q7aDefault
        , benchmarkNative "q7n" c "q7.sql"
        ]
    , B.bgroup "Q8"
        [ benchmarkDSH "q8" c q8Default
        , benchmarkNative "q8n" c "q8.sql"
        ]
    , B.bgroup "Q9"
        [ benchmarkDSH "q9" c q9Default
        , benchmarkNative "q9n" c "q9.sql"
        ]
    , B.bgroup "Q10"
        [ benchmarkDSH "q10" c q10Default
        , benchmarkNative "q10n" c "q10.sql"
        ]
    , B.bgroup "Q11"
        [ benchmarkDSH "q11" c q11Default
        , benchmarkNative "q11n" c "q11.sql"
        ]
    , B.bgroup "Q12"
        [ benchmarkDSH "q12" c q12Default
        , benchmarkDSH "q12a" c q12aDefault
        , benchmarkNative "q12n" c "q12.sql"
        ]
    , B.bgroup "Q13"
        [ benchmarkDSH "q13" c q13Default
        , benchmarkNative "q13n" c "q13.sql"
        ]
    , B.bgroup "Q14"
        [ benchmarkDSH "q14" c q14Default
        , benchmarkDSH "q14a" c q14aDefault
        , benchmarkNative "q14n" c "q14.sql"
        ]
    , B.bgroup "Q15"
        [ benchmarkDSH "q15" c q15Default
        , benchmarkNative "q15n" c "q15.sql"
        ]
    , B.bgroup "Q16"
        [ benchmarkDSH "q16" c q16
        , benchmarkNative "q16n" c "q16.sql"
        ]
    , B.bgroup "Q17"
        [ benchmarkDSH "q17" c q17
        , benchmarkNative "q17n" c "q17.sql"
        ]
    , B.bgroup "Q18"
        [ benchmarkDSH "q18" c q18Default
        , benchmarkNative "q18n" c "q18.sql"
        ]
    , B.bgroup "Q19"
        [ benchmarkDSH "q19" c q19Default
        , benchmarkDSH "q19a" c q19aDefault
        , benchmarkNative "q19n" c "q19.sql"
        ]
    , B.bgroup "Q20"
        [ benchmarkDSH "q20" c q20Default
        , benchmarkNative "q20n" c "q20.sql"
        ]
    , B.bgroup "Q21"
        [ benchmarkDSH "q21" c q21Default
        , benchmarkDSH "q21a" c q21aDefault
        , benchmarkDSH "q21b" c q21bDefault
        , benchmarkDSH "q21c" c q21cDefault
        , benchmarkNative "q21n" c "q21.sql"
        ]
    , B.bgroup "Q22"
        [ benchmarkDSH "q22" c q22Default
        , benchmarkNative "q22n" c "q22.sql"
        ]
    ]

main :: IO ()
main = do
    c <- O.connectODBC "DSN=tpch1test"
    M.defaultMain $ benchmarks c
