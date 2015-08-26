{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad
import           Text.Printf

import qualified System.Directory         as D
import           System.Environment
import           System.FilePath          ((</>))
import           System.IO.Error

import qualified Database.DSH             as Q
import qualified Database.DSH.Backend.Sql as DS
import qualified Database.DSH.Compiler    as C

import qualified Queries.Shredding.Paper  as Shredding
import qualified Queries.TPCH.NonStandard as Nested
import qualified Queries.TPCH.Standard    as TPCH

ifNecessary :: IO () -> IO ()
ifNecessary ma = catchJust (\e -> if isAlreadyExistsError e then Just () else Nothing)
                           ma
                           (const (return ()))

dumpTPCH :: IO ()
dumpTPCH = do
    ifNecessary (D.createDirectory "tpch")
    dumpSql "tpch" TPCH.q1Default "dsh_tpch_q1"
    dumpSql "tpch" TPCH.q2Default "dsh_tpch_q2"
    dumpSql "tpch" TPCH.q3Default "dsh_tpch_q3"
    dumpSql "tpch" TPCH.q4Default "dsh_tpch_q4"
    dumpSql "tpch" TPCH.q5Default "dsh_tpch_q5"
    dumpSql "tpch" TPCH.q6Default "dsh_tpch_q6"
    dumpSql "tpch" TPCH.q7Default "dsh_tpch_q7"
    dumpSql "tpch" TPCH.q8Default "dsh_tpch_q8"
    dumpSql "tpch" TPCH.q9Default "dsh_tpch_q9"
    dumpSql "tpch" TPCH.q10Default "dsh_tpch_q10"
    dumpSql "tpch" TPCH.q11Default "dsh_tpch_q11"
    dumpSql "tpch" TPCH.q12Default "dsh_tpch_q12"
    dumpSql "tpch" TPCH.q13Default "dsh_tpch_q13"
    dumpSql "tpch" TPCH.q14Default "dsh_tpch_q14"
    dumpSql "tpch" TPCH.q15Default "dsh_tpch_q15"
    dumpSql "tpch" TPCH.q16 "dsh_tpch_q16"
    dumpSql "tpch" TPCH.q17 "dsh_tpch_q17"
    dumpSql "tpch" TPCH.q18Default "dsh_tpch_q18"
    dumpSql "tpch" TPCH.q19Default "dsh_tpch_q19"
    dumpSql "tpch" TPCH.q20Default "dsh_tpch_q20"
    dumpSql "tpch" TPCH.q21aDefault "dsh_tpch_q21"
    dumpSql "tpch" TPCH.q22Default "dsh_tpch_q22"

dumpShredding :: IO ()
dumpShredding = do
    ifNecessary (D.createDirectory "shredding")
    dumpSql "shredding" Shredding.q1 "dsh_shredding_q1"
    dumpSql "shredding" Shredding.q2 "dsh_shredding_q2"
    dumpSql "shredding" Shredding.q3 "dsh_shredding_q3"
    dumpSql "shredding" Shredding.q4 "dsh_shredding_q4"
    dumpSql "shredding" Shredding.q5 "dsh_shredding_q5"
    dumpSql "shredding" Shredding.q6 "dsh_shredding_q6"

dumpNested :: IO ()
dumpNested = do
    ifNecessary (D.createDirectory "nested")
    dumpSql "nested" (Nested.custFromOrders "GERMANY") "dsh_custfromorders"
    dumpSql "nested" (Nested.custRevenues "GERMANY") "dsh_custrevenues"
    dumpSql "nested" (Nested.expectedRevenueFor "GERMANY") "dsh_expectedrevenuefor"
    dumpSql "nested" Nested.shippingDelay "dsh_shippingdelay"
    dumpSql "nested" Nested.shippingDelayInterval "dsh_shippingdelayinterval"
    dumpSql "nested" (Nested.topOrdersPerCust' 10 "GERMANY") "dsh_toporderspercust"
    dumpSql "nested" (Nested.regionsTopCustomers "EUROPE" 10) "dsh_regionstopcustomers"
    dumpSql "nested" (Nested.unshippedItemsPerCustomer "GERMANY") "dsh_unshippeditemspercustomer"
    dumpSql "nested" (Nested.cheaperSuppliersInRegion "EUROPE") "dsh_cheapersuppliersinregion"
    dumpSql "nested" (Nested.cheaperSuppliersInRegionAvg "EUROPE") "dsh_cheapersuppliersinregionavg"

dumpSql :: Q.QA a => FilePath -> Q.Q a -> FilePath -> IO ()
dumpSql category q qName = do
    let dirPath = category </> qName
    D.createDirectory dirPath
    let backendQueries = map DS.unwrapCode $ C.codeQ (undefined :: DS.SqlBackend) q

    let sqlPath i = dirPath </> printf "%s_%d.sql" qName i
    let namedQs = zipWith (\bq i -> (sqlPath i, bq)) backendQueries ([1..] :: [Int])
    mapM_ (uncurry writeFile) namedQs
    mapM_ putStrLn $ map fst namedQs

main :: IO ()
main = do
    args <- getArgs
    when ("tpch" `elem` args) dumpTPCH
    when ("shredding" `elem` args) dumpShredding
    when ("nested" `elem` args) dumpNested
