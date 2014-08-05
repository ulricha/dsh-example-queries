module Main where

import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.Csv
import qualified Data.Foldable        as F
import           Data.IORef
import           Data.Sequence        (Seq, (<|), (><))
import qualified Data.Sequence        as Seq
import qualified Data.Set             as S
import qualified Data.Traversable     as T
import           Data.Vector          ((!), (//))
import qualified Data.Vector          as V
import           System.IO
import           System.Random.MWC

flowInterrupt :: Int
flowInterrupt = 120

data Options = Options
    { o_hosts      :: !Int
    , o_conns      :: !Int
    , o_avgFlows   :: !Int
    , o_avgFlowLen :: !Int
    , o_gen        :: GenIO
    , o_file       :: Handle
    , o_pidSrc     :: IORef Int
    }

type Host      = Int
type Timestamp = Int

data Packet  = P
    { p_pid :: !Int
    , p_src :: !Host
    , p_dst :: !Host
    , p_len :: !Int
    , p_ts  :: !Timestamp
    }

instance ToRecord Packet where
    toRecord (P pid src dst len ts) =
        record [toField pid, toField src, toField dst, toField len, toField ts ]

genHosts :: Int -> V.Vector Host
genHosts n = V.enumFromN 1 n

genHostIdxs :: Options -> IO (S.Set (Int, Int))
genHostIdxs opts = go S.empty
  where
    go :: S.Set (Host, Host) -> IO (S.Set (Int, Int))
    go s | S.size s == (o_conns opts) = return s
    go s | otherwise                  = do
        src <- uniformR (0, (o_hosts opts) - 1) (o_gen opts)
        dst <- uniformR (0, (o_hosts opts) - 1) (o_gen opts)
        go (S.insert (src, dst) s)

genHostPairs :: Options -> V.Vector Host -> IO (V.Vector (Host, Host))
genHostPairs opts hosts = do
    idxs <- genHostIdxs opts
    return $ fmap lookupHosts $ initVec // (zip [0..] (S.toList idxs))

  where
    initVec :: V.Vector (Host, Host)
    initVec = V.replicate (o_conns opts) (0, 0)

    lookupHosts :: (Int, Int) -> (Host, Host)
    lookupHosts (src, dst) = (hosts ! src, hosts ! dst)

writePackets :: Handle -> Seq Packet -> IO ()
writePackets f ps = B.hPut f $ encode $ F.toList ps

-- | Generate and write out all flows for a given pair of src and
-- destination hosts
writeFlows :: Options -> Host -> Host -> IO ()
writeFlows opts src dst = do
    startTs <- uniformR (0, 2^31) (o_gen opts)

    nrFlowFactor <- uniformR (0.5 :: Double, 1.5) (o_gen opts)
    let nrFlows = round $ (fromIntegral $ o_avgFlows opts) * nrFlowFactor

    packets <- genFlows opts src dst startTs Seq.empty nrFlows
    writePackets (o_file opts) packets

-- | Generate all flows for given src and dest and collect the
-- packets.
genFlows :: Options -> Host -> Host -> Timestamp -> Seq Packet -> Int -> IO (Seq Packet)
genFlows _    _   _   _       ps 0 = return ps
genFlows opts src dst startTs ps n = do
    (lastTs, flow) <- genFlow opts startTs src dst
    startVar <- uniformR (1, 1000) (o_gen opts)
    let nextStart = lastTs + flowInterrupt + startVar
    genFlows opts src dst nextStart (flow >< ps) (n - 1)


-- | Generate one flow between two hosts: Vary the average flow length
-- and collect the packets for the flow. The function returns the
-- packets and the timestamp of the last packet.
genFlow :: Options -> Timestamp -> Host -> Host -> IO (Timestamp, Seq Packet)
genFlow opts flowStart src dst = do
    flowLenFactor <- uniformR (0.3 :: Double, 1.3) (o_gen opts)
    let flowLen = round $ (fromIntegral $ o_avgFlowLen opts) * flowLenFactor

    genPackets opts flowStart src dst Seq.empty flowLen



-- | Generate packets for a flow: Compute a random offset between
-- packets that is smaller than the flow interruption constant
-- (120). The function returns the packets and the timestamp of the
-- last packet.
genPackets :: Options -> Timestamp -> Host -> Host -> Seq Packet -> Int -> IO (Timestamp, Seq Packet)
genPackets _    ts _   _   ps 0 = return (ts, ps)
genPackets opts ts src dst ps n = do
    packetLen  <- uniformR (5, 500) (o_gen opts)

    pid        <- readIORef (o_pidSrc opts)
    modifyIORef' (o_pidSrc opts) (+ 1)

    let packet = P pid src dst packetLen ts

    tsOffset   <- uniformR (5, 80) (o_gen opts)
    let nextTs = ts + tsOffset

    genPackets opts nextTs src dst (packet <| ps) (n - 1)

main :: IO ()
main = do
    gen    <- withSystemRandom $ asGenIO $ return
    f      <- openFile "packets.csv" WriteMode
    pidSrc <- newIORef (0 :: Int)
    let opts = Options 5000 10000 15 1000 gen f pidSrc
    let hosts = genHosts (o_hosts opts)
    conns <- genHostPairs opts hosts
    void $ T.mapM (uncurry (writeFlows opts)) conns
    hClose f
