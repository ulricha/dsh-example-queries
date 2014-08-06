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

module Queries.AQuery.Packets(flowStats) where

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

sums :: (QA a, Num a) => Q [a] -> Q [a]
sums as = [ sum [ a' | (view -> (a', i')) <- nas, i' <= i ]
          | let nas = number as
	  , (view -> (a, i)) <- nas
	  ]

-- | For each packet, compute the ID of the flow that it belongs to
flowids :: Q [Packet] -> Q [Integer]
flowids ps = sums [ if d > 120 then 1 else 0 | d <- deltas $ map p_tsQ ps ]

-- | For each flow, compute the number of packets and average length
-- of packets in the flow. A flow is defined as a number of packets
-- between the same source and destination in which the time gap
-- between consecutive packets is smaller than 120ms.
flowStats :: Q [(Integer, Integer, Integer, Double)]
flowStats = [ tuple4 src 
                     dst 
                     (length g)
                     (avg $ map (p_lenQ . fst) g)
            | (view -> (k, g)) <- flows
            , let (view -> (src, dst, _)) = k
            ]
  where
    flows = groupWithKey (\p -> tuple3 (p_srcQ $ fst p) (p_destQ $ fst p) (snd p)) 
                         $ zip packetsOrdered (flowids packetsOrdered)

    packetsOrdered = sortWith (\p -> tuple3 (p_srcQ p) (p_destQ p) (p_tsQ p)) packets
