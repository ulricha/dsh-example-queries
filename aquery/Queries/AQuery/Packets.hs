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
    ( flowStatsDrop
    , flowStatsSelf
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

deltas :: Q [Integer] -> Q [Integer]
deltas xs = cons 0 (map (\(view -> (a, b)) -> a - b) (zip (drop 1 xs) xs))

-- TRY OUT: better or worse than drop?
deltas' :: Q [Integer] -> Q [Integer]
deltas' xs = [ ts - ts'
             | (view -> (ts, i))   <- number xs
             , (view -> (ts', i')) <- number xs
             , i' == i - 1
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
    [ tuple4 src 
             dst 
             (length g)
             (avg $ map (p_lenQ . fst) g)
    | (view -> (k, g)) <- flows
    , let (view -> (src, dst, _)) = k
    ]
  where
    flows = groupWithKey (\p -> tuple3 (p_srcQ $ fst p) (p_destQ $ fst p) (snd p)) 
                         $ zip packetsOrdered (flowids deltaFun packetsOrdered)

    packetsOrdered = sortWith (\p -> tuple3 (p_srcQ p) (p_destQ p) (p_tsQ p)) packets

flowStatsDrop :: Q [(Integer, Integer, Integer, Double)]
flowStatsDrop = flowStats deltas

flowStatsSelf :: Q [(Integer, Integer, Integer, Double)]
flowStatsSelf = flowStats deltas' 

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
