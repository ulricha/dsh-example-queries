-- | Generate data for the trading example from Shasha and Lerner's AQuery VLDB paper.
module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy  as B
import qualified Data.Csv              as C
import qualified Data.Foldable         as F
import           Data.Sequence         (Seq, (<|))
import qualified Data.Sequence         as Seq
import           System.Console.GetOpt
import           System.Environment
import           System.IO
import           System.Random.MWC
import qualified Data.Time.Calendar as C
import           Data.Int

msPerDay :: Int64
msPerDay = 1000 * 3600 * 24

startDate :: Day
startDate = Day $ C.fromGregorian 2015 11 6

type Timestamp = Int64
type ID        = Int

newtype Day = Day C.Day

instance Enum Day where
    toEnum n = Day $ toEnum n
    fromEnum (Day d) = fromEnum d

instance C.ToField Day where
    toField (Day d) = C.toField $ C.showGregorian d

instance Show Day where
    show (Day d) = show d

data Trade = Trade
    { t_price :: {-# UNPACK #-} !Double
    , t_tid   :: {-# UNPACK #-} !ID
    , t_ts    :: {-# UNPACK #-} !Timestamp
    , t_date  ::  !Day
    }

instance C.ToRecord Trade where
    toRecord (Trade p sid ts date) =
        C.record [ C.toField p, C.toField sid, C.toField ts, C.toField date ]

{-

Parameters:

- Number of days
- Number of stocks
- Average number of trades per day and stock
- Every item is traded every day
- resolution per day: microsecond --> [1, 864000]
-}

data Options = Options
    { o_days      :: !Int       -- ^ Number of days
    , o_stocks    :: !Int       -- ^ Number stocks
    , o_avgTrades :: !Int       -- ^ Average trades per day and
                                -- stock
    , o_gen       :: GenIO
    , o_file      :: Handle
    }

mkDefaultOptions :: IO Options
mkDefaultOptions = do
    gen    <- withSystemRandom $ asGenIO return
    f      <- openFile "trades.csv" WriteMode

    return $ Options { o_days       = 30
                     , o_stocks     = 1000
                     , o_avgTrades  = 10000
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
        (_, _, errs) -> ioError (userError $ concat errs ++ usageInfo msg options)
  where
    msg = "Usage: tradegen [OPTION...]"

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
writeTrades opts trades = B.hPut (o_file opts) $ C.encode $ F.toList trades

genTrades :: Options -> IO ()
genTrades opts = do

    let days   = zip [0..] (take (o_days opts) [startDate..])
        stocks = [0 .. o_stocks opts]

    forM_ days $ \day -> do
        putStrLn $ "day " ++ show day

        let genStockTrades (stock : ss) acc = do
                tradesFactor <- uniformR (0.7 :: Double, 1.3) (o_gen opts)
                let nrTrades = round $ tradesFactor * (fromIntegral $ o_avgTrades opts)
                let startTs = fst day * msPerDay
                trades <- genDayStockTrades opts (snd day) stock Seq.empty startTs nrTrades
                genStockTrades ss (trades Seq.>< acc)

            genStockTrades [] acc = return acc

        trades <- genStockTrades stocks Seq.empty

        writeTrades opts trades

genDayStockTrades :: Options -> Day -> ID -> Seq Trade -> Timestamp -> Int -> IO (Seq Trade)
genDayStockTrades _    _   _     trades _      0 = return trades
genDayStockTrades opts day stock trades nextTs n = do
    price    <- uniformR (1 :: Double, 10000) (o_gen opts)
    let trade = Trade price stock nextTs day
    tsOffset <- uniformR (1, 10) (o_gen opts) :: IO Int
    genDayStockTrades opts day stock (trade <| trades) (nextTs + (fromIntegral tsOffset)) (n - 1)

main :: IO ()
main = do
    argv <- getArgs
    opts <- parseOptions argv
    genTrades opts
    hClose (o_file opts)
