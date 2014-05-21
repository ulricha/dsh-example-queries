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

module DFT.Radix 
    ( radix2Fft
    ) where

import Debug.Trace

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import DFT.Complex
import DFT.Basic
import DFT.Buneman

---------------------------------------------------------------------
-- Special case: Radix-2 FFT, directly transcribed from the Wikipedia
-- (imperative) pseudocode.

appendPair :: QA a => Q ([a], [a]) -> Q [a]
appendPair (view -> (as, bs)) = as ++ bs

radix2Fft :: Integer -> Q [Complex] -> Q [Complex]
radix2Fft n v | 2 P.* n P.<= 32 = dft (2 P.* n) v
radix2Fft n v | otherwise       = trace (show n) $
    appendPair $ unzip $ zipWith3 combine twiddles evens odds
  where
    idxs     = toQ [0 .. n-1]
    twiddles = map (ω (2 * n)) idxs
    evens    = take (toQ n) subFfts
    odds     = drop (toQ n) subFfts
    subFfts  = concat $ map (radix2Fft (n `P.div` 2)) $ φ $ ρ 2 v
    combine t e o = pair (e .+ t .* o) (e .- t .* o)
