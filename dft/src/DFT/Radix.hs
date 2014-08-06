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
    , radix4Fft
    , radix4FftShare
    ) where

import Debug.Trace

import qualified Prelude as P
import Database.DSH
import Database.DSH.Compiler

import DFT.Support
import DFT.Complex
import DFT.Basic
import DFT.Buneman

---------------------------------------------------------------------
-- Special case: Radix-2 FFT (DIT), directly transcribed from the
-- Wikipedia (imperative) pseudocode:
-- http://en.wikipedia.org/wiki/Cooley%E2%80%93Tukey_FFT_algorithm

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

---------------------------------------------------------------------
-- Special case: Radix-4 FFT (DIT).
-- Source DSP Book, CRC Press 2000

unzip4 :: (QA a, QA b, QA c, QA d) => Q [(a, b, c, d)] -> Q ([a], [b], [c], [d])
unzip4 abcds = tuple4 (map (\e -> case view e of (a, _, _, _) -> a) abcds)
                      (map (\e -> case view e of (_, b, _, _) -> b) abcds)
                      (map (\e -> case view e of (_, _, c, _) -> c) abcds)
                      (map (\e -> case view e of (_, _, _, d) -> d) abcds)

zip5 :: (QA a, QA b, QA c, QA d, QA e)
         => Q [a] 
         -> Q [b] 
         -> Q [c] 
         -> Q [d] 
         -> Q [e] 
         -> Q [(a, b, c, d, e)]
zip5 as bs cs ds es = map flatten (zip as (zip bs (zip cs (zip ds es))))
  where
    flatten e = tuple5 (fst e)
                       (fst (snd e))
                       (fst (snd (snd e)))
                       (fst (snd (snd (snd e))))
                       (snd (snd (snd (snd e))))

appendFour :: QA a => Q ([a], [a], [a], [a]) -> Q [a]
appendFour (view -> (as1, as2, as3, as4)) = as1 ++ as2 ++ as3 ++ as4

zipWith5 :: (QA a, QA b, QA c, QA d, QA e, QA r) 
         => (Q a -> Q b -> Q c -> Q d -> Q e -> Q r) 
         -> Q [a] 
         -> Q [b] 
         -> Q [c] 
         -> Q [d] 
         -> Q [e] 
         -> Q [r]
zipWith5 f as bs cs ds es = map (\e -> case view e of (a, b, c, d, e) -> f a b c d e) 
                                (zip5 as bs cs ds es)

radix4Fft :: Integer -> Q [Complex] -> Q [Complex]
radix4Fft n v | 4 P.* n P.<= 32 = trace ("dft4 " P.++ show (n P.* 4)) $ dft (4 P.* n) v
radix4Fft n v | otherwise       = trace ("fft4 " P.++ show (n P.* 4)) $
    appendFour $ unzip4 $ zipWith5 combine twiddles ys zs gs hs 
  where
    idxs     = indexes n
    twiddles = map (\r -> triple (ω n r) (ω n (2 * r)) (ω n (3 * r))) idxs
    subFfts  = map (radix4Fft $ n `P.div` 4) $ φ $ ρ 4 v
    ys       = subFfts !! 0
    zs       = subFfts !! 1
    gs       = subFfts !! 2
    hs       = subFfts !! 3
    combine t y z g h =
        let (t1, t2, t3) = view t
            xr1 = y .+ t1 .* z .+ t2 .* g .+ t3 .* h
            xr2 = y .- t1 .* z .+ t2 .* g .+ t3 .* h
            xr3 = y .+ t1 .* z .- t2 .* g .+ t3 .* h
            xr4 = y .+ t1 .* z .+ t2 .* g .- t3 .* h
        in tuple4 xr1 xr2 xr3 xr4

-- HACK HACK HACK. Use a comprehension with a singleton generator to
-- force sharing of the radix4 sub-ffts. This works only as long as
-- M-Norm-2 is disabled.
radix4FftShare :: Integer -> Q [Complex] -> Q [Complex]
radix4FftShare n v | 4 P.* n P.<= 32 = trace ("dft4 " P.++ show (n P.* 4)) $ dft (4 P.* n) v
radix4FftShare n v | otherwise       = trace ("fft4 " P.++ show (n P.* 4)) $
    concat $ [ appendFour $ unzip4 $ zipWith5 combine twiddles (xs !! 0) (xs !! 1) (xs !! 2) (xs !! 3)
             | xs <- singleton subFfts
             ]
  where
    idxs     = indexes n
    twiddles = map (\r -> triple (ω n r) (ω n (2 * r)) (ω n (3 * r))) idxs
    subFfts  = map (radix4Fft $ n `P.div` 4) $ φ $ ρ 4 v

    combine t y z g h =
        let (t1, t2, t3) = view t
            xr1 = y .+ t1 .* z .+ t2 .* g .+ t3 .* h
            xr2 = y .- t1 .* z .+ t2 .* g .+ t3 .* h
            xr3 = y .+ t1 .* z .- t2 .* g .+ t3 .* h
            xr4 = y .+ t1 .* z .+ t2 .* g .- t3 .* h
        in tuple4 xr1 xr2 xr3 xr4

-- Try this instead of reshape/transpose for shuffling: Might make a
-- difference when recursion gets deeper and inputs longer.

shuffle4 :: Q [Complex] -> Q [[Complex]]
shuffle4 vec = zeros <| ones <| twos <| singleton threes

  where
    zeros  = [ c | (view -> (c, p)) <- vecPos, p `mod` 4 == 0 ]
    ones   = [ c | (view -> (c, p)) <- vecPos, p `mod` 4 == 1 ]
    twos   = [ c | (view -> (c, p)) <- vecPos, p `mod` 4 == 2 ]
    threes = [ c | (view -> (c, p)) <- vecPos, p `mod` 4 == 3 ]

    vecPos = number0 vec

-- Bind the numbered vector in a generator to avoid term explosion.
shuffle4' :: Q [Complex] -> Q [[Complex]]
shuffle4' vec = map filterPos $ toQ [0..3]

  where
    filterPos i = [ c | (view -> (c, p)) <- vecPos, p `mod` 4 == i ]

    vecPos = number0 vec
