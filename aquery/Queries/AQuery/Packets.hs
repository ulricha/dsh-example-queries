{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MonadComprehensions   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE ViewPatterns          #-}

module Queries.AQuery.Packets
    ( flowStatsZip
    , flowStatsSelfJoin
    , flowStatsWin
    ) where

import qualified Prelude as P
import Database.DSH

--------------------------------------------------------------------------------
-- Schema definition

data Packet = Packet
    { p_dest :: Integer
    , p_len  :: Integer
    , p_pid  :: Integer
    , p_src  :: Integer
    , p_ts   :: Integer
    }

deriveDSH ''Packet
deriveTA ''Packet
generateTableSelectors ''Packet

packets :: Q [Packet]
packets = table "packets" $ TableHints [ Key ["p_pid"]] NonEmpty

--------------------------------------------------------------------------------
-- Flow statistics

-- | Positional aligning via zip
deltasZip :: Q [Integer] -> Q [Integer]
deltasZip xs = cons 0 (map (\(view -> (a, b)) -> a - b) (zip (drop 1 xs) xs))
-- deltasZip xs = cons 0 [ a - b | a <- drop 1 xs | b <- xs ]

-- | Aligning with an explicit (order-preserving) self join
deltasSelfJoin :: Q [Integer] -> Q [Integer]
deltasSelfJoin xs = cons 0 [ ts - ts'
                           | (view -> (ts, i))   <- number xs
                           , (view -> (ts', i')) <- number xs
                           , i' == i - 1
                           ]

-- | Aligning using a nested self join. Note that this is semantically
-- not equivalent to the other deltas: For each element we compute the
-- minimum of the element and its predecessor. If the input is ordered
-- by timestamps at least in a partitioned way, this will be OK.
deltasWin :: Q [Integer] -> Q [Integer]
deltasWin xs = [ ts - minimum [ ts' 
                             | (view -> (ts', i')) <- number xs
                             , i' >= i - 1
                             , i' <= i
                             ]
              | (view -> (ts, i)) <- number xs
              ]

deltasHead :: Q [Integer] -> Q [Integer]
deltasHead xs = [ ts - head [ ts' 
                            | (view -> (ts', i')) <- number xs
                            , i' >= i - 1
                            , i' <= i
                            ]
                | (view -> (ts, i)) <- number xs
                ]

sums :: (QA a, Num a) => Q [a] -> Q [a]
sums as = [ sum [ a' | (view -> (a', i')) <- nas, i' <= i ]
          | let nas = number as
	  , (view -> (a, i)) <- nas
	  ]

-- | For each packet, compute the ID of the flow that it belongs to
flowids :: (Q [Integer] -> Q [Integer]) -> Q [Packet] -> Q [Integer]
flowids deltaFun ps = sums [ if d > 120 then 1 else 0 | d <- deltaFun $ map p_tsQ ps ]

-- | For each flow, compute the number of packets and average length
-- of packets in the flow. A flow is defined as a number of packets
-- between the same source and destination in which the time gap
-- between consecutive packets is smaller than 120ms.
flowStats :: (Q [Integer] -> Q [Integer]) -> Q [(Integer, Integer, Integer, Double)]
flowStats deltaFun = 
    [ tup4 src 
           dst 
           (length g)
           (avg $ map (p_lenQ . fst) g)
    | (view -> (k, g)) <- flows
    , let (view -> (src, dst, _)) = k
    ]
  where
    flows = groupWithKey (\p -> tup3 (p_srcQ $ fst p) (p_destQ $ fst p) (snd p)) 
                         $ zip packetsOrdered (flowids deltaFun packetsOrdered)

    packetsOrdered = sortWith (\p -> tup3 (p_srcQ p) (p_destQ p) (p_tsQ p)) packets

{-

Cleaned up for presentation, the query could look like this:

flowStats :: (Q [Integer] -> Q [Integer]) -> Q [(Integer, Integer, Integer, Double)]
flowStats = 
    [ (src, dst, length g, avg $ map (p_lenQ . fst) g)
    | ((src, dst, _), g) <- flows
    ]
  where
    flows = groupWith (\(p, fid) -> (p_srcQ p, p_destQ p, fid) )
                      $ zip packetsOrdered (flowids packetsOrdered)

    packetsOrdered = sortWith (\p -> (p_srcQ p, p_destQ p, p_tsQ p)) packets
-}

flowStatsZip :: Q [(Integer, Integer, Integer, Double)]
flowStatsZip = flowStats deltasZip

flowStatsSelfJoin :: Q [(Integer, Integer, Integer, Double)]
flowStatsSelfJoin = flowStats deltasSelfJoin

flowStatsWin :: Q [(Integer, Integer, Integer, Double)]
flowStatsWin = flowStats deltasWin

--------------------------------------------------------------------------------
-- Different formulation

type FlowID = Integer

{-
-- Assume packets ordered by p_ts
precedingPacket :: Q [Packet] -> Q [(Packet, [Packet])]
precedingPacket ps = 
    [ pair p [ p' 
             | (view -> (p', i')) <- number ps
             , p_srcQ p == p_srcQ p'
             , p_destQ p == p_destQ p'
             , i' >= i - 1 && i' <= i
             ]
    | (view -> (p, i)) <- number ps
    ]

flowids' :: Q [Packet] -> Q [(Packet, FlowID)]
flowids' ps = 
  map (\(view -> (a, i)) -> pair (fst a) i) 
  $ sums' [ let diff = p_tsQ p - (min $ map p_tsQ pps)
            in if diff > 120
               then pair p 1
               else else p 0
          | (view -> (p, pps)) <- precedingPacket ps ]

sums' :: (QA a, QA b, Num b) => Q [a] -> (Q a -> Q b) -> Q [(a, b)]
sums' as p = [ pair a (sum [ p a' | (view -> (a', i')) <- nas, i' <= i ])
             | let nas = number as
             , (view -> (a, i)) <- nas
             ]
             
flowStats' :: Q [(Integer, Integer, Integer, Double)]
flowStats' = [ tuple4 src 
                      dst 
                      (length g)
                      (avg $ map (p_lenQ . fst) g)
             | (view -> (k, g)) <- flows
             , let (view -> (src, dst, _)) = k
             ]
  where
    flows = groupWithKey (\(view -> (p, fid)) -> triple (p_srcQ p) (p_destQ p) fid)
                         $ flowids' packetsOrdered

    packetsOrdered = sortWith p_tsQ packets


foo :: Q [[Packet]] -> Q a
foo pss = map number pss

-}

{-

partitionHosts :: Q [Packet] -> Q [[Packet]]
partitionHosts ps = 
    [ pair p [ p' | p' <- ps, p_srcQ p == p_srcQ p', p_destQ p == p_destQ p' ]
    | p <- ps
    ]

mins1 :: (Ord a, QA a) => Q [a] -> Q [a]
mins1 as = [ minimum [ a' | (view -> (a', i')) <- nas, i' >= i - 1, i' <= i]
           | let nas = number as
	   , (view -> (a, i)) <- nas
	   ]   

flowids :: 

map flowids         -- 
$ map mins1         -- Q [[Integer]]
$ map (map p_tsQ)   -- Q [[Integer]]
$ partitionHosts ps -- Q [[Packet]]

-- Need: Packet + min (ideally without zipping


allPrec :: QA a => Q [a] -> Q [(a, [a])]
allPrec as = [ pair a [ a' | (view -> (a', i')) <- number as, i' <= i ]
             | (view -> (a, i) <- number as 
             ]

onePrec :: QA a => Q [a] -> Q [(a, [a])]

-}
