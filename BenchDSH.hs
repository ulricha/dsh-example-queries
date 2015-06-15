{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

-- | Benchmark TPC-H queries executed using DSH against native SQL queries
-- executed using HDBC.
module Main where

import           Control.Monad

import           Control.DeepSeq          (NFData)
import qualified Criterion                as B
import qualified Criterion.Main           as M
import qualified Data.ByteString.Char8    as BS
import qualified Data.Convertible         as Conv
import qualified Data.Decimal             as D
import qualified Database.HDBC            as H
import qualified Database.HDBC.ODBC       as O

import qualified Database.DSH             as Q
import qualified Database.DSH.Backend.Sql as S
import qualified Database.DSH.Compiler    as C

import           Queries.TPCH

-- provide sql text in environment.
-- prepare query on every go.
-- Produce list result with the same type as DSH query.

--------------------------------------------------------------------------------
-- The row types of query results for the native SQL queries

-- FIXME HACK HACK HACK
instance Conv.Convertible H.SqlValue D.Decimal where
    safeConvert (H.SqlRational d)   = Right $ D.realFracToDecimal 5 d
    safeConvert (H.SqlByteString c) = Right $ read $ BS.unpack c
    safeConvert v                   =
        Left $ Conv.ConvertError { Conv.convSourceValue  = show v
                                 , Conv.convSourceType   = "SqlValue"
                                 , Conv.convDestType     = "Decimal"
                                 , Conv.convErrorMessage = "conversion failed"
                                 }

q1Row :: [H.SqlValue] -> ( Char, Char, Q.Decimal, Q.Decimal
                         , Q.Decimal, Q.Decimal, Q.Decimal, Q.Decimal, (Q.Decimal, Integer))
q1Row [rf, ls, sq, bp, dp, c, q, p, d, o] = (,,,,,,,,) (Conv.convert rf)
                                                       (Conv.convert ls)
                                                       (Conv.convert sq)
                                                       (Conv.convert bp)
                                                       (Conv.convert dp)
                                                       (Conv.convert c)
                                                       (Conv.convert q)
                                                       (Conv.convert p)
                                                       (Conv.convert d, Conv.convert o)
q1Row _                                   = error "q1: row format"

q2Row :: [H.SqlValue] -> (Q.Decimal, Q.Text, Q.Text, Integer, Q.Text, Q.Text, Q.Text, Q.Text)
q2Row [a, sn, nn, pk, m, ad, p, c] = (,,,,,,,) (Conv.convert a)
                                               (Conv.convert sn)
                                               (Conv.convert nn)
                                               (Conv.convert pk)
                                               (Conv.convert m)
                                               (Conv.convert ad)
                                               (Conv.convert p)
                                               (Conv.convert c)
q2Row _                            = error "q2: row format"

q3Row :: [H.SqlValue] -> (Integer, Q.Decimal, Integer, Q.Day)
q3Row [k, r, d, p] = (,,,) (Conv.convert k) (Conv.convert r) (Conv.convert d) (Conv.convert p)
q3Row _            = error "q3: row format"

q4Row :: [H.SqlValue] -> (Q.Text, Integer)
q4Row [p, c] = (,) (Conv.convert p) (Conv.convert c)
q4Row _      = error "q4: row format"

q5Row :: [H.SqlValue] -> (Q.Text, Q.Decimal)
q5Row [n, r] = (,) (Conv.convert n) (Conv.convert r)
q5Row _      = error "q5: row format"

q6Row :: [H.SqlValue] -> Q.Decimal
q6Row [r] = Conv.convert r
q6Row _   = error "q6: row format"

q7Row :: [H.SqlValue] -> (Q.Text, Q.Text, Integer, Q.Decimal)
q7Row [s, c, y, r] = (,,,) (Conv.convert s) (Conv.convert c) (Conv.convert y) (Conv.convert r)
q7Row _            = error "q7: row format"

q8Row :: [H.SqlValue] -> (Integer, Q.Decimal)
q8Row [y, v] = (,) (Conv.convert y) (Conv.convert v)
q8Row _      = error "q8: row format"

q9Row :: [H.SqlValue] -> (Q.Text, Integer, Q.Decimal)
q9Row [n, y, p] = (,,) (Conv.convert n) (Conv.convert y) (Conv.convert p)
q9Row _         = error "q9: row format"

q10Row :: [H.SqlValue] -> (Integer, Q.Text, Q.Decimal, Q.Decimal, Q.Text, Q.Text, Q.Text, Q.Text)
q10Row [c, n, r, a, n2, a2, p, c2] = (,,,,,,,) (Conv.convert c)
                                               (Conv.convert n)
                                               (Conv.convert r)
                                               (Conv.convert a)
                                               (Conv.convert n2)
                                               (Conv.convert a2)
                                               (Conv.convert p)
                                               (Conv.convert c2)
q10Row _                           = error "q10: row format"

q11Row :: [H.SqlValue] -> (Integer, Q.Decimal)
q11Row [p, v] = (,) (Conv.convert p) (Conv.convert v)
q11Row _      = error "q11: row format"

q12Row :: [H.SqlValue] -> (Q.Text, Integer, Integer)
q12Row [m, h, l] = (,,) (Conv.convert m) (Conv.convert h) (Conv.convert l)
q12Row _         = error "q12: row format"

q13Row :: [H.SqlValue] -> (Integer, Integer)
q13Row [c, d] = (,) (Conv.convert c) (Conv.convert d)
q13Row _      = error "q13: row format"

q14Row :: [H.SqlValue] -> Q.Decimal
q14Row [r] = Conv.convert r
q14Row _   = error "q14: row format"

q15Row :: [H.SqlValue] -> (Integer, Q.Text, Q.Text, Q.Text, Q.Decimal)
q15Row [k, n, a, p, r] = (,,,,) (Conv.convert k)
                                (Conv.convert n)
                                (Conv.convert a)
                                (Conv.convert p)
                                (Conv.convert r)
q15Row _               = error "q15: row format"

q16Row :: [H.SqlValue] -> (Q.Text, Q.Text, Integer, Integer)
q16Row [p, t, s, c] = (,,,) (Conv.convert p) (Conv.convert t) (Conv.convert s) (Conv.convert c)
q16Row _            = error "q16: row format"

q17Row :: [H.SqlValue] -> Q.Decimal
q17Row [r] = Conv.convert r
q17Row _   = error "q17: row format"

q18Row :: [H.SqlValue] -> (Q.Text, Integer, Integer, Q.Day, Q.Decimal, Q.Decimal)
q18Row [n, c, k, d, p, q] = (,,,,,) (Conv.convert n)
                                    (Conv.convert c)
                                    (Conv.convert k)
                                    (Conv.convert d)
                                    (Conv.convert p)
                                    (Conv.convert q)
q18Row _                  = error "q18: row format"

q19Row :: [H.SqlValue] -> Q.Decimal
q19Row [r] = Conv.convert r
q19Row _   = error "q19: row format"

q20Row :: [H.SqlValue] -> (Q.Text, Q.Text)
q20Row [n, a] = (,) (Conv.convert n) (Conv.convert a)
q20Row _      = error "q20: row format"

q21Row :: [H.SqlValue] -> (Q.Text, Integer)
q21Row [n, c] = (,) (Conv.convert n) (Conv.convert c)
q21Row _      = error "q21: row format"

q22Row :: [H.SqlValue] -> (Q.Text, Integer, Q.Decimal)
q22Row [c, n, a] = (,,) (Conv.convert c) (Conv.convert n) (Conv.convert a)
q22Row _         = error "q22: row format"

--------------------------------------------------------------------------------
-- Execute benchmarks

runHDBC :: O.Connection -> String -> ([H.SqlValue] -> a) -> IO [a]
runHDBC c q mkRow = do
    stmt <- H.prepare c q
    void $ H.execute stmt []
    rows <- H.fetchAllRows' stmt
    return $ map mkRow rows


-- | Benchmark the complete execution of a DSH query
benchmarkDSH :: (Q.QA a, NFData a)
             => String
             -> O.Connection
             -> Q.Q a
             -> B.Benchmark
benchmarkDSH benchName c q = B.bench benchName $ B.nfIO (C.runQ (S.sqlBackend c) q)

-- | Benchmark a SQL query obtained from a file.
benchmarkNative :: NFData a
                => String
                -> O.Connection
                -> FilePath
                -> ([H.SqlValue] -> a)
                -> B.Benchmark
benchmarkNative benchName c q mkRow =
    B.env (readFile $ "tpch_native/" ++ q)
          (\q -> B.bench benchName $ B.nfIO (runHDBC c q mkRow))

--------------------------------------------------------------------------------
-- Benchmark definition

benchmarks :: O.Connection -> [B.Benchmark]
benchmarks c =
    [ B.bgroup "Q1"
        [ benchmarkDSH "q1" c q1Default
        , benchmarkNative "q1n" c "q1.sql" q1Row
        ]
    , B.bgroup "Q2"
        [ benchmarkDSH "q2" c q2Default
        -- , benchmarkDSH "q2a" c q2aDefault
        , benchmarkNative "q2n" c "q2.sql" q2Row
        ]
    , B.bgroup "Q3"
        [ benchmarkDSH "q3" c q3Default
        -- , benchmarkDSH "q3a" c q3Default
        -- , benchmarkDSH "q3b" c q3Default
        , benchmarkNative "q3n" c "q3.sql" q3Row
        ]
    , B.bgroup "Q4"
        [ benchmarkDSH "q4" c q4Default
        -- , benchmarkDSH "q4a" c q4aDefault
        -- , benchmarkDSH "q4b" c q4bDefault
        , benchmarkNative "q4n" c "q4.sql" q4Row
        ]
    , B.bgroup "Q5"
        [ benchmarkDSH "q5" c q5Default
        , benchmarkNative "q5n" c "q5.sql" q5Row
        ]
    , B.bgroup "Q6"
        [ benchmarkDSH "q6" c q6Default
        , benchmarkNative "q6n" c "q6.sql" q6Row
        ]
    , B.bgroup "Q7"
        [ benchmarkDSH "q7" c q7Default
        -- , benchmarkDSH "q7a" c q7aDefault
        , benchmarkNative "q7n" c "q7.sql" q7Row
        ]
    , B.bgroup "Q8"
        [ benchmarkDSH "q8" c q8Default
        , benchmarkNative "q8n" c "q8.sql" q8Row
        ]
    , B.bgroup "Q9"
        [ benchmarkDSH "q9" c q9Default
        , benchmarkNative "q9n" c "q9.sql" q9Row
        ]
    , B.bgroup "Q10"
        [ benchmarkDSH "q10" c q10Default
        , benchmarkNative "q10n" c "q10.sql" q10Row
        ]
    , B.bgroup "Q11"
        [ benchmarkDSH "q11" c q11Default
        , benchmarkNative "q11n" c "q11.sql" q11Row
        ]
    , B.bgroup "Q12"
        [ benchmarkDSH "q12" c q12Default
        , benchmarkDSH "q12a" c q12aDefault
        , benchmarkNative "q12n" c "q12.sql" q12Row
        ]
    , B.bgroup "Q13"
        [ benchmarkDSH "q13" c q13Default
        , benchmarkNative "q13n" c "q13.sql" q13Row
        ]
    , B.bgroup "Q14"
        [ benchmarkDSH "q14" c q14Default
        , benchmarkDSH "q14a" c q14aDefault
        , benchmarkNative "q14n" c "q14.sql" q14Row
        ]
    , B.bgroup "Q15"
        [ benchmarkDSH "q15" c q15Default
        , benchmarkNative "q15n" c "q15.sql" q15Row
        ]
    , B.bgroup "Q16"
        [ benchmarkDSH "q16" c q16
        , benchmarkNative "q16n" c "q16.sql" q16Row
        ]
    , B.bgroup "Q17"
        [ benchmarkDSH "q17" c q17
        , benchmarkNative "q17n" c "q17.sql" q17Row
        ]
    , B.bgroup "Q18"
        [ benchmarkDSH "q18" c q18Default
        , benchmarkNative "q18n" c "q18.sql" q18Row
        ]
    , B.bgroup "Q19"
        [ benchmarkDSH "q19" c q19Default
        -- , benchmarkDSH "q19a" c q19aDefault
        , benchmarkNative "q19n" c "q19.sql" q19Row
        ]
    , B.bgroup "Q20"
        [ benchmarkDSH "q20" c q20Default
        , benchmarkNative "q20n" c "q20.sql" q20Row
        ]
    , B.bgroup "Q21"
        [ benchmarkDSH "q21" c q21Default
        -- , benchmarkDSH "q21a" c q21aDefault
        -- , benchmarkDSH "q21b" c q21bDefault
        -- , benchmarkDSH "q21c" c q21cDefault
        , benchmarkNative "q21n" c "q21.sql" q21Row
        ]
    , B.bgroup "Q22"
        [ benchmarkDSH "q22" c q22Default
        , benchmarkNative "q22n" c "q22.sql" q22Row
        ]
    ]

main :: IO ()
main = do
    c <- O.connectODBC "DSN=tpch1test"
    M.defaultMain $ benchmarks c
