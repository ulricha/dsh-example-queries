-- | Benchmark execution of shredding queries via DSH
module Main where

import           Control.Monad
import           System.IO
import           Text.Printf

import           Control.DeepSeq          (NFData)
import qualified Criterion                as CB
import qualified Criterion.Measurement    as CM
import qualified Criterion.Types          as CT
import qualified Database.HDBC.ODBC       as O

import qualified Database.DSH             as Q
import qualified Database.DSH.Backend.Sql as S
import qualified Database.DSH.Compiler    as C

import           Queries.Shredding.Paper

-- | Benchmark the complete execution of a DSH query
benchmarkDSH :: (Q.QA a, NFData a)
             => O.Connection
             -> Q.Q a
             -> CB.Benchmarkable
benchmarkDSH c q = CB.nfIO (C.runQ (S.sqlBackend c) q)

benchmarks :: O.Connection -> [(String, CB.Benchmarkable)]
benchmarks c =
    [ ("q1", benchmarkDSH c q1)
    , ("q2", benchmarkDSH c q2)
    , ("q3", benchmarkDSH c q3)
    , ("q4", benchmarkDSH c q4)
    , ("q5", benchmarkDSH c q5)
    , ("q6", benchmarkDSH c q6)
    ]

writeResults :: [Double] -> FilePath -> IO ()
writeResults rs f = withFile f WriteMode $ \h -> do
    forM_ rs $ \r -> hPutStrLn h (show r)

resFileName :: String -> FilePath
resFileName p = printf "shredding_%s_raw.txt" p

runTimes :: CB.Benchmarkable -> Int -> IO [Double]
runTimes b n = forM [1..n] $ \i -> do
    putStrLn $ show i
    CT.measTime <$> fst <$> CM.measure b 1

bench :: CB.Benchmarkable -> Int -> String -> IO ()
bench b n p = do
    putStrLn p 
    rs <- runTimes b n
    writeResults rs (resFileName p)

main :: IO ()
main = do
    c <- O.connectODBC "DSN=organisation4096"
    CM.initializeTime
    forM_ (benchmarks c) $ \(p, b) -> bench b 10 p
