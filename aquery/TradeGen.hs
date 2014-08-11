module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy  as B
import           Data.Csv
import qualified Data.Foldable         as F
import           Data.IORef
import           Data.Sequence         (Seq, (<|), (><))
import qualified Data.Sequence         as Seq
import qualified Data.Set              as S
import qualified Data.Traversable      as T
import           Data.Vector           ((!), (//))
import qualified Data.Vector           as V
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Random.MWC

msPerDay :: Int
msPerDay = 10 * 3600 * 24

type Timestamp = Int
type Date      = Int
type ID        = Int

data Trade = Trade
    { t_price :: Real
    , t_tid   :: ID
    , t_ts    :: Timestamp
    , t_date  :: Day
    }

instance ToRecord Trade where
    toRecord (Trade p id ts date) = 
        record [ toField p, toField id, toField ts, toField date ]

genDays :: Int -> V.Vector Day

{-

Parameters:

- Number of days
- Number of stocks
- Average number of trades per day and stock
- Every item is traded every day
- resolution per day: millisecond --> [1, 864000]
-}

data Options = Options
    { o_days      :: !Int       -- ^ Number of days
    , o_stocks    :: !Int       -- ^ Number stocks
    , o_avgTrades :: !Int       -- ^ Average trades per day and
                                -- stock
    , o_gen       :: GenIO
    , o_file      :: Handle
    , o_pidSrc    :: IORef Int
    }

mkDefaultOptions :: IO Options
mkDefaultOptions = do
    gen    <- withSystemRandom $ asGenIO return
    f      <- openFile "trades.csv" WriteMode

    return $ Options { o_days       = 365
                     , o_stocks     = 1000
                     , o_avgTrades  = 6000
                     , o_gen        = gen
                     , o_file       = f
                     }

options :: [OptDescr (Options -> IO Options)]
options =
    [ Option ['d'] ["days"]
             (ReqArg (\hs opts -> return $ opts { o_days = read hs } ) "DAYS")
             "number of days"
    , Option ['s'] ["stocks"]
             (ReqArg (\cs opts -> return $ opts { o_stocks = read cs } ) "STOCKS")
             "number of stocks that are traded"
    , Option ['t'] ["avgtrades"]
             (ReqArg (\fs opts -> return $ opts { o_avgTrades = read fs } ) "TRADES")
             "average number of trades per day and stock"
    , Option ['o'] ["outfile"]
             (ReqArg (\fname opts -> hClose (o_file opts)
                                       >> openFile fname WriteMode
                                       >>= \h -> return $ opts { o_file = h })
                     "FILE")
             "output file"
    ]

parseOptions :: [String] -> IO Options
parseOptions argv =
    case getOpt Permute options argv of
        (o, [], [])  -> mkDefaultOptions >>= \defOpt -> F.foldlM (flip id) defOpt o
        (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo header options)
  where
    header = "Usage: tradegen [OPTION...]"

{-
for each day d:
    offset = d * 86400
    for each stock s:
        scale average trades
        for each trade t:
            determine random offset to last trade time
            generate random (?) price
            write out
-}

writeTrades :: Options -> Seq Trade -> IO ()
writeTrades opts trades = B.hPut (o_file opts) $ encode $ T.toList trades

genTrades :: Options -> IO ()
genTrades opts = do

    let days   = V.enumFromN 0 (o_days opts)
        stocks = V.enumFromN 0 (o_stocks opts)
    
    T.forM_ days (\day ->
        T.forM_ stocks (\stock -> do
            tradesFactor <- uniformR (0.7 :: Double, 1.3) (o_gen opts)
            let trades   = round $ tradesFactor * (fromIntegral $ o_avgTrades opts)
                tsOffset = day * msPerDay

            (trades, _) <- F.foldlM (genDayStockTrades opts day stock) (Seq.empty, tsOffset) trades
            writeTrades opts trades

genDayStockTrades :: Options -> Day -> Stock -> Seq Trade -> Timestamp -> Int -> IO (Seq Trade)
genDayStockTrades opts day stock trades nextTs 0 = return trades
genDayStockTrades opts day stock trades nextTs n = do
    price    <- uniformR (1 :: Int, 10000) (o_gen opts)
    let trade = Trade price stock nextTs day
    tsOffset <- uniformR (1, 10) (o_gen opts)
    genDayStockTrades opts day stock (trade |> trades) (nextTs + tsOffset) (n - 1)

main :: IO ()
main = do
    argv <- getArgs
    opts <- parseOptions argv
    genTrades opts
    hClose (o_file opts)
